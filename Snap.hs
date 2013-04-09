{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- address messages to specific clients in order to change session, etc
-- better names for objects so you can getOrCreate "ball", but they're
-- namespaced by instance

-- can't do uploads and stuff from elm (fay?)
-- check out bacon.js
-- https://github.com/raimohanska/bacon.js/wiki/Documentation
-- use elm example to make a bacon websocket

module Snap where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.FilePath

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

import TCP

--import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent (forkIO, killThread)
import Control.Exception (finally)

import qualified Network.WebSockets as WS
import Network.WebSockets.Snap

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.IxSet as Ix

import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Attoparsec as PB
import qualified Data.Aeson.Parser as P
import qualified Data.Aeson as A
import Data.Aeson (encode)

import Data.Maybe (fromJust)


import Types
import Actions



mkResponse :: Packet -> Result -> Packet
mkResponse p (Error v) = p { pPayload = Nothing, pError = Just v }
mkResponse p (OK v _)  = p { pPayload = Just v }

runPacket env = do
    res <- dispatch env
    notify env res
    return . Just $ mkResponse (pPacket env) res
   
dispatch PacketEnv{..} = dispatch' pEndpoint pAction
  where
    Packet{..}    = pPacket
    ServerEnv{..} = pServerEnv

    -- Object commands
    dispatch' ["o", oid] "create"     = createObject oid pPayload sObjects
    dispatch' ["o", oid] "get"        = getObject oid sObjects
    dispatch' ["o", oid] "set"        = mergeObject oid pPayload sObjects
    dispatch' ["o", oid] "inc"        = incObject oid pPayload sObjects
    dispatch' ["o"]      "sub"        = subObjects pClient sSubs

    -- User commands
    dispatch' ["u"]      "get"        = getUsers sUsers
    dispatch' ["u", uid] "connect"    = connectUser uid pClient sUsers
    dispatch' ["u", uid] "disconnect" = disconnectUser uid sUsers
    dispatch' ["u", uid] "msg"        = msgUser uid pPayload sUsers

    -- File commands
    dispatch' ("f":p)    "get"        = listFiles p
    dispatch' ("f":p)    "delete"     = deleteFile p

    dispatch' _ _ = return $ Error (A.String "No such endpoint")
    

-------------------------------------------------------------------------------
-- Web API

webAPI env = do
    action <- parseAction
    packet <- Packet (Just 1) <$> (parseEndpoint <$> getSafePath) 
                              <*> return action
                              <*> (parsePayload  <$> readRequestBody 4096)
                              <*> return Nothing
    mpacket <- liftIO $ runPacket $ PacketEnv env httpClient packet
    respond' mpacket
  where
    parseEndpoint = map T.pack . splitDirectories
    parsePayload bs = decodeStrictValue $ toStrict bs
    respond' = writeJSON . fromJust

parseAction :: Snap Text
parseAction = do
    -- try querystring first
    ma <- getQueryParam "a"
    case ma of
      Just x -> return . T.toLower . decodeUtf8 $ x
      Nothing -> do
         -- if that fails, use default action from Method
         method' <- rqMethod <$> getRequest
         case method' of
           GET    -> return "get"
           POST   -> return "set"
           DELETE -> return "delete"
           _      -> pass   

-------------------------------------------------------------------------------
-- WebSocket API

wsAPI :: ServerEnv -> WS.Request -> WS ()
wsAPI env rq = do
    WS.acceptRequest rq
    go
  where
    go = do
        n <- WS.receiveData :: WS B.ByteString
        case decodeStrict n of
          Nothing -> return () -- discard bad packet
          Just p  -> do
              mpacket <- liftIO $ runPacket $ PacketEnv env httpClient p
              WS.sendTextData . encode . fromJust $ mpacket
        go

-------------------------------------------------------------------------------
-- Socket API

runSocketServer env =
    serveFork (Host "127.0.0.1") "8001" $ \(sock, _remoteAddr) ->
      flip finally (cleanup sock) $ 
          socketDecoder sock $ \p -> do
              mpacket <- runPacket $ PacketEnv env (client sock) p
              sendAll sock . toStrict . A.encode . fromJust $ mpacket
  where
    client sock = Client "socket" $ SocketClient sock
    cleanup _ = return ()
        
-- | looped socket JSON decoder
socketDecoder :: A.FromJSON a => Socket-> (a -> IO ()) -> IO ()
socketDecoder sock f = loop Nothing Nothing
  where 
    loop mpartial mbytes = do
        bytes <- maybe (recv sock 4096) return mbytes
        unless (B.null bytes) $ -- null recv indicates closed connection
          case maybe (PB.parse A.json') PB.feed mpartial bytes of
            PB.Fail _ _ _reason -> loop Nothing Nothing
            k@PB.Partial{}      -> loop (Just k) Nothing
            PB.Done bytes' c    -> do
                case A.fromJSON c of
                  A.Success a -> f a
                  _           -> return ()
                loop Nothing $ if B.null bytes' 
                                 then Nothing 
                                 else Just bytes'

-------------------------------------------------------------------------------
-- Main

writeJSON :: A.ToJSON a => a -> Snap ()
writeJSON a = do 
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . A.encode $ a

config = 
    setPort 8000 .
    setVerbose False .
    setErrorLog  (ConfigIoLog B.putStrLn) .
    setAccessLog ConfigNoLog $
    defaultConfig 

runWebServer :: ServerEnv -> IO ()
runWebServer env = httpServe config (route routes) where
    routes = [ ("/ws", setTimeout 31536000 >> runWebSocketsSnap (wsAPI env))
             , ("/api", webAPI env)
             , ("/files", serveDirectory "files")
             , ("/", serveDirectory "static")
             ]

main :: IO ()
main = do
    env <- ServerEnv <$> newMVar M.empty
                     <*> newMVar M.empty 
                     <*> newMVar Ix.empty
    tid1 <- forkIO $ runWebServer env
    tid2 <- forkIO $ runSocketServer env
    getLine
    killThread tid1
    killThread tid2


-------------------------------------------------------------------------------
-- Parsing Utils

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

decodeStrictValue :: B.ByteString -> Maybe A.Value
decodeStrictValue bs = 
    case PB.parseOnly P.value bs of
      Left _  -> Nothing
      Right v -> Just v

-- | strict Aeson decoding
decodeStrict :: A.FromJSON a => B.ByteString -> Maybe a
decodeStrict bs = 
    case PB.parseOnly P.json' bs of
      Left _  -> Nothing
      Right v -> case A.fromJSON v of
                   A.Success a -> Just a
                   _           -> Nothing

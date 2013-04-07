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

--import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative

-- socket -> parse json packet -> process it -> possibly send socket response
-- so the pipe proxy includes a client reference so we know if a response is
-- necessary

import Network.WebSockets (WebSockets, Hybi10)
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap

import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Attoparsec as PB
import qualified Data.Aeson.Parser as P
import qualified Data.Aeson as A
import Data.Aeson (ToJSON, FromJSON, decode', encode)


import Data.Maybe (fromJust, isJust)

import Types
import Actions


writeJSON :: (A.ToJSON a, MonadSnap m) => a -> m ()
writeJSON a = do 
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . A.encode $ a

config = 
    setPort 8000 .
    setVerbose False .
    setErrorLog  (ConfigIoLog B.putStrLn) .
    setAccessLog ConfigNoLog $
    defaultConfig 

runWebServer :: Env -> IO ()
runWebServer env = httpServe config (route routes) where
    routes = [ 
              -- ("/ws", setTimeout 31536000 >> runWebSocketsSnap app)
             ("/api", api env)
             , ("/", serveDirectory "static")
             ]


sendNotifications (UserUpdated uid) = undefined

notify (Error _) = return ()
notify (OK _ n)  = iatom $ sendNotifications n

mkResponse :: Packet -> Result -> Packet
mkResponse p (Error v) = p { pPayload = Nothing, pError = Just v }
mkResponse p (OK v _)  = p { pPayload = Just v }

emptyPacket = Packet Nothing [] Nothing Nothing

runPacket :: NGON (Maybe Packet)
runPacket = do
  (_, _, packet) <- ask
  res <- dispatch (pEndpoint packet)
  notify res
  return $ Just $ mkResponse packet res
    

iatom = liftIO . atomically

dispatch :: [EndpointComponent] -> NGON Result
dispatch ep = do
  (Env{..}, client, packet) <- ask
  case ep of
    ["u", uid, "login"] -> iatom $ tryLogin uid client eUsers




api env = do
    -- TODO: urlDecode rqPathInfo
    endpoint <- parseEndpoint <$> getRequest
    payload  <- parsePayload <$> readRequestBody 4096
    res <- liftIO $ runNGON (env, HTTPClient, mkPacket endpoint payload) runPacket
    writeJSON . encode . fromJust $ res
  where
    mkPacket e p = Packet (Just 1) e p Nothing
    parseEndpoint = map T.strip . T.split (=='/') . decodeUtf8 . rqPathInfo
    parsePayload bs = case PB.parseOnly P.value (toStrict bs) of
                        Left _  -> Nothing
                        Right v -> Just v
      

toStrict = B.concat . BL.toChunks


main :: IO ()
main = do
    env <- Env <$> newTVarIO M.empty
    runWebServer env


{-
decodePackets :: (Monad m, Proxy p) => () -> Pipe p ByteString Packet m r
decodePackets () = runIdentityP $ forever $ do
    b <- request ()
    case AP.parse A.json' b of
      AP.Done _ v -> case A.fromJSON v of
                       A.Success a -> respond a
                       _           -> return ()
      _           -> return ()

app :: WS.Request -> WebSockets Hybi10 ()
app rq = do
    WS.acceptRequest rq
    go
  where
    go = do
        n <- WS.receiveData :: WebSockets Hybi10 ByteString
        liftIO $ print n
        go
-}

-- TODO: how do I get the sink out?
{-
readWebSocketS :: Proxy p => () -> Consumer p ByteString Snap ()
readWebSocketS () = runIdentityP $
    lift $ runWebSocketsSnap $ \rq -> WS.acceptRequest rq >> go
      where 
        go = do
            n <- WS.receiveData :: WebSockets Hybi10 ByteString
            embed $ respond n
            go
-}
            

    


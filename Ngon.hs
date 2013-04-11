{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- fix discarded extra bytes after connect

-- check out bacon.js
-- https://github.com/raimohanska/bacon.js/wiki/Documentation
-- use elm example to make a bacon websocket

module Main where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.FilePath
import System.Directory

import System.Posix.Signals
import System.Exit (ExitCode(..))

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

import qualified Network.WebSockets as WS
import Network.WebSockets.Snap

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Exception (finally)

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

import TCP
import Types
import Actions


-------------------------------------------------------------------------------
-- Packet Processing

-- | Initial packet handler for new clients. Only accepts packets with endpoint
-- "u/username" and attempts to connect them. Loops until a successful
-- connection and then returns a 'Client' object.
runConnectPacket :: ServerEnv -> ClientHandle -> Packet -> IO (Maybe Client)
runConnectPacket env chandle p@Packet{..} =
    case pEndpoint of
        ["u", uid] -> do
            let client = Client (UId uid) chandle
            ok <- connectUser client (sUsers env)
            if ok
              then do
                  sendConnect uid 
                  when (hasId p) $ 
                      sendPacket client $ p { pPayload = Just true }
                  return $ Just client
              else do
                  when (hasId p) $ 
                      sendPacket client $ addError "User id already in use" p
                  return Nothing
        _ -> do
            let client = Client (UId "unknown") chandle
            sendPacket client $ addError "Connect first" p
            return Nothing
  where
    sendConnect uid = sendNotifications env 
                        (Packet Nothing ["u", uid] "connect" Nothing Nothing)
                        [AllUsersSub]

-- | Packet handler for connected clients.
runPacket :: PacketEnv -> IO (Maybe Packet)
runPacket env = do
    res <- dispatch env
    notify env res
    return $ if hasId p
               then Just $ mkResponse res
               else Nothing
  where 
    p = pPacket env
    mkResponse (Error v) = p { pPayload = Nothing, pError = Just v }
    mkResponse (OK _ v)  = p { pPayload = Just v }

dispatch :: PacketEnv -> IO Result
dispatch env@PacketEnv{..} = d pEndpoint pAction
  where
    Packet{..}    = pPacket
    Client{..}    = pClient
    ServerEnv{..} = pServerEnv

    -- Object commands
    d ["o"] "get"             = okResult <$> getObjects sObjects
    d ["o", oid] "create"     = createObject (OId oid) pPayload sObjects
    d ["o", oid] "get"        = maybeResult "No such object id" $ getObject (OId oid) sObjects
    d ["o", oid] "set"        = mergeObject cUId (OId oid) pPayload sObjects sLocks
    d ["o", oid] "inc"        = incObject cUId (OId oid) pPayload sObjects sLocks

    d ["o", oid, prop] "get"  = getObjectProp (OId oid) prop sObjects 
    d ["o", oid, prop] "set"  = setObjectProp cUId (OId oid) prop pPayload sObjects sLocks
    d ["o", oid, prop] "inc"  = incObjectProp cUId (OId oid) prop pPayload sObjects sLocks

    d ["o", oid] "lock"       = lockObject cUId (OId oid) sObjects sLocks
    d ["o", oid] "unlock"     = unlockObject cUId (OId oid) sObjects sLocks

    -- User commands
    d ["u"]      "get"        = okResult <$> getUsers sUsers
    d ["u", uid] "set"        = msgUser pPacket (UId uid) pClient sUsers

    -- File commands
    d ("f":p)    "get"        = okResult <$> listFiles (mkPath p)
    d ("f":p)    "delete"     = deleteFile (mkPath p)

    -- Subscription commands
    d ["o"]      "sub"        = addSub env AllObjectsSub
    d ["o", oid] "sub"        = addSub env (ObjectSub (OId oid))
    d ["u"]      "sub"        = addSub env AllUsersSub
    d ("f":p)    "sub"        = addSub env (FileSub $ mkPath p)

    d ["o"]      "unsub"      = unSub env AllObjectsSub
    d ["o", oid] "unsub"      = unSub env (ObjectSub (OId oid))
    d ["u"]      "unsub"      = unSub env AllUsersSub
    d ("f":p)    "unsub"      = unSub env (FileSub $ mkPath p)

    d _ _                     = return $ errorResult "No such endpoint"
    

-------------------------------------------------------------------------------
-- Web API

webAPI env = do
    packet <- Packet (Just 1) <$> (parseEndpoint <$> getSafePath) 
                              <*> parseAction
                              <*> parsePayload
                              <*> return Nothing
    rpacket <- (fromJust <$>) . liftIO $ runPacket $ PacketEnv env httpClient packet
    writeJSON $ mplus (pError rpacket) (pPayload rpacket)
  where
    parseEndpoint = map T.pack . splitDirectories

writeJSON :: A.ToJSON a => a -> Snap ()
writeJSON a = do 
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . A.encode $ a

parsePayload :: Snap (Maybe A.Value)
parsePayload = do
    -- try querystring first
    mp <- getQueryParam "p"
    case mp of
      Just x -> return $ decodeStrictValue x
      Nothing -> decodeStrictValue . toStrict <$> readRequestBody 4096

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

handleUploads :: ServerEnv -> Snap ()
handleUploads env = method POST $ do
      (tmp, rel, full) <- getPaths
      mkDirP full
      handleFileUploads tmp policy 
          (const $ allowWithMaximumSize maxSize) $ 
              mapM_ (handler rel full)
  where
    getPaths = do
        tmp <- liftIO getTemporaryDirectory
        pwd <- liftIO getCurrentDirectory
        rel <- getSafePath
        return (tmp, rel, pwd </> fileRoot </> rel)
    mkDirP    = liftIO . createDirectoryIfMissing True
    maxSize   = 1024 * 1000 * 100
    packet fp = Packet Nothing (fpToEndpoint fp) "create" Nothing Nothing
    policy    = setMaximumFormInputSize maxSize defaultUploadPolicy
    handler rel full (info, res) = case res of
        Left _    -> return ()
        Right tfp -> case partFileName info of  
            Nothing -> return ()
            Just fn -> liftIO $ do
                renameFile tfp (full </> fn')
                sendNotifications env (packet $ rel </> fn') 
                  [FileSub rel]
              where fn' = B.unpack fn

-------------------------------------------------------------------------------
-- WebSocket API

wsAPI :: ServerEnv -> WS.Request -> WS ()
wsAPI env rq = do
    WS.acceptRequest rq
    sink <- WS.getSink
    client <- webSocketDecoder $ runConnectPacket env (WebSocketClient sink)
    flip WS.catchWsError (handler client) $
        webSocketDecoder $ \p -> do
            mrpacket <- runPacket $ PacketEnv env client p
            maybe (return ()) (WS.sendSink sink . WS.textData . encode) mrpacket
            return Nothing
  where
    handler client e = liftIO $ do
        print e 
        disconnectUser (cUId client) env

-- | loop parsing JSON as long as action returns Nothing
webSocketDecoder :: A.FromJSON a => (a -> IO (Maybe b)) -> WS b
webSocketDecoder action = loop
  where 
    loop = do
        n <- WS.receiveData :: WS B.ByteString
        case decodeStrict n of
          Nothing -> loop
          Just p  -> do
              res <- liftIO $ action p
              case res of
                Nothing -> loop
                Just b  -> return b

-------------------------------------------------------------------------------
-- Socket API

runSocketServer env@ServerEnv{..} =
    serveFork (Host "127.0.0.1") "8001" $ \(sock, _remoteAddr) -> do
        (bs, mclient) <- socketDecoder sock Nothing $ 
            runConnectPacket env (SocketClient sock)
        case mclient of
          Nothing     -> return () -- disconnected
          Just client -> flip finally (cleanup client) $ 
              void $ socketDecoder sock (Just bs) $ \p -> do
                  mrpacket <- runPacket $ PacketEnv env client p
                  maybe (return ()) (sendAll sock . toStrict . encode) mrpacket
                  return Nothing
  where
    cleanup client = disconnectUser (cUId client) env
        
-- | loop parsing JSON as long as action returns Nothing
socketDecoder :: A.FromJSON a => Socket 
              -> Maybe B.ByteString
              -> (a -> IO (Maybe b)) -> IO (B.ByteString, Maybe b)
socketDecoder sock bs action = loop Nothing bs
  where 
    loop mpartial mbytes = do
        bytes <- maybe (recv sock 4096) return mbytes
        if B.null bytes
          then return (B.empty, Nothing)
          else case maybe (PB.parse A.json') PB.feed mpartial bytes of
            PB.Fail _ _ _reason -> loop Nothing Nothing
            k@PB.Partial{}      -> loop (Just k) Nothing
            PB.Done bytes' v    -> do
                mb <- case A.fromJSON v of
                             A.Success a -> action a
                             _           -> return Nothing
                case mb of
                  Just b  -> 
                      -- return any extra bytes along with the result
                      return (bytes', Just b)
                  Nothing -> loop Nothing $ if B.null bytes' 
                                              then Nothing 
                                              else Just bytes'
                  
-------------------------------------------------------------------------------
-- Main

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
             , ("/files", handleUploads env)
             , ("/files", serveDirectory fileRoot)
             , ("/", serveDirectory "static")
             ]

main :: IO ()
main = do
    env <- ServerEnv <$> newMVar M.empty
                     <*> newMVar M.empty 
                     <*> newMVar Ix.empty
                     <*> newMVar Ix.empty
    t1 <- myThreadId
    t2 <- forkIO $ runWebServer env
    t3 <- forkIO $ runSocketServer env
    installHandler sigINT (Catch $ sigINThandler [t3,t2,t1]) Nothing
    forever $ threadDelay 1000000

sigINThandler :: [ThreadId] -> IO ()
sigINThandler = mapM_ (`throwTo` ExitSuccess)

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

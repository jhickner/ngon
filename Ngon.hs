{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, RecordWildCards #-}

-- recv error still happens when sending from socket, so it doesn't originate
-- from http call
-- seems to be caused by websockets disconnecting

module Main where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.FilePath
import System.Directory

import System.Posix.Signals

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

import qualified Network.WebSockets as WS
import Network.WebSockets.Snap

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Monad.CatchIO as CIO
import Control.Applicative
import Control.Concurrent
import Control.Exception (SomeException, fromException, finally, handle)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.IxSet as Ix

import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as B

import qualified Data.Attoparsec as PB
import qualified Data.Aeson as A
import Data.Aeson (encode)

import Data.Maybe (fromJust)

import TCP
import Utils
import Types
import Actions


-------------------------------------------------------------------------------
-- Main / Server

main :: IO ()
main = do
    env <- ServerEnv <$> newMVar M.empty
                     <*> newMVar M.empty 
                     <*> newMVar Ix.empty
                     <*> newMVar Ix.empty
    t  <- myThreadId
    t1 <- forkIO $ runWebServer env
    t2 <- forkIO $ runSocketServer env
    installHandler sigINT (Catch $ sigINThandler [t, t1, t2]) Nothing
    putStrLn "NGON starting up..."
    forever $ threadDelay 1000000
  where sigINThandler = mapM_ killThread

runWebServer :: ServerEnv -> IO ()
runWebServer env = httpServe config (route routes) 
  where
    routes = [ ("/ws",    ws)
             , ("/api",   httpAPI env)
             , ("/files", handleUploads env)
             , ("/files", serveDirectory fileRoot)
             , ("/",      serveDirectory "static")
             ]
    ws = do
        setTimeout 31536000
        runWebSocketsSnap $ wsAPI env

config :: Config Snap a
config = 
    setPort 8000 .
    setVerbose False .
    setErrorLog  (ConfigIoLog B.putStrLn) .
    -- setErrorLog  ConfigNoLog .
    setAccessLog ConfigNoLog $
    defaultConfig 

-------------------------------------------------------------------------------
-- HTTP API

httpAPI :: ServerEnv -> Snap ()
httpAPI env = do
    packet <- Packet Nothing <$> (parseEndpoint <$> getSafePath) 
                             <*> parseAction
                             <*> parsePayload
                             <*> return Nothing
    rpacket <- (fromJust <$>) . liftIO $ runPacket $ mkPacketEnv env httpClient packet
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

-------------------------------------------------------------------------------
-- Upload handler

handleUploads :: ServerEnv -> Snap ()
handleUploads env@ServerEnv{..} = method POST $ do
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
            mrpacket <- runPacket $ mkPacketEnv env client p
            maybe (return ()) (WS.sendSink sink . WS.textData . encode) mrpacket
            return Nothing
  where
    handler client e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ disconnectUser (cUId client) env
        _ -> liftIO $ print e

-- | loop parsing JSON as long as action returns Nothing
webSocketDecoder :: A.FromJSON a => (a -> IO (Maybe b)) -> WS b
webSocketDecoder action = loop
  where 
    loop = do
        n <- WS.receiveData
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
    serveFork (Host "0.0.0.0") "8001" $ \(sock, _remoteAddr) -> do
        (bs, mclient) <- socketDecoder sock Nothing $ 
            runConnectPacket env (SocketClient sock)
        case mclient of
          Nothing     -> return () -- disconnected
          Just client -> flip finally (cleanup client) $ 
              void $ socketDecoder sock (Just bs) $ \p -> do
                  mrpacket <- runPacket $ mkPacketEnv env client p
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
-- Packet Processing

-- | Initial packet handler for new clients. Only accepts packets with endpoint
-- "u/username" and attempts to connect them. Loops until a successful
-- connection and then returns a 'Client' object.
runConnectPacket :: ServerEnv -> ClientHandle -> Packet -> IO (Maybe Client)
runConnectPacket env@ServerEnv{..} chandle p@Packet{..} =
    case pEndpoint of
        ["u", uid] -> do
            let client = Client (UId uid) chandle
            isOK <- connectUser client sUsers
            if isOK
              then do
                  putStrLn $ "[" ++ show client ++ "] - " ++ show p
                  sendConnect uid 
                  when (hasId p) $ 
                      sendPacket env client p
                  return $ Just client
              else do
                  when (hasId p) $ 
                      sendPacket env client $ addE p "User id already in use"
                  return Nothing
        _ -> do
            sendHandlePacket chandle $ addE p "Connect first"
            return Nothing
  where
    sendConnect uid = sendNotifications env
                        (Packet Nothing ["u", uid] "connect" Nothing Nothing)
                        [AllUsersSub]

debug PacketEnv{..} =
  putStrLn $  "[" ++ show pClient ++"] - " ++ show pPacket

-- | Packet handler for connected clients.
runPacket :: PacketEnv -> IO (Maybe Packet)
runPacket env = do
    debug env
    res <- dispatch env
    notify env res
    return $ if hasId (pPacket env) || isHTTP (pClient env)
               then Just $ getP res
               else Nothing
  where 
    getP (Error p) = p
    getP (OK _ p)  = p

dispatch :: PacketEnv -> IO Result
dispatch env@PacketEnv{..} = d pEndpoint pAction
  where
    Packet{..} = pPacket
    Client{..} = pClient

    -- Object commands
    d ["o"] "get"             = okResult pPacket <$> getObjects env
    d ["o", oid] "create"     = createObject (OId oid) pPayload env
    d ["o", oid] "delete"     = deleteObject cUId (OId oid) env
    d ["o", oid] "get"        = maybeResult pPacket "No such object id" $ 
                                  getObject (OId oid) env
    d ["o", oid] "set"        = mergeObject cUId (OId oid) pPayload env
    d ["o", oid] "inc"        = incObject cUId (OId oid) pPayload env

    d ["o", oid, prop] "get"  = getObjectProp (OId oid) prop env
    d ["o", oid, prop] "set"  = setObjectProp cUId (OId oid) prop pPayload env
    d ["o", oid, prop] "inc"  = incObjectProp cUId (OId oid) prop pPayload env

    d ["o", oid] "lock"       = lockObject cUId (OId oid) env
    d ["o", oid] "unlock"     = unlockObject cUId (OId oid) env

    -- User commands
    d ["u"]      "get"        = okResult pPacket <$> getUsers env
    d ["u", uid] "set"        = msgUser (UId uid) env
    d ["u"]      "set"        = msgAllUsers env

    -- File commands
    d ("f":p)    "get"        = okResult pPacket <$> listFiles (mkPath p)
    d ("f":p)    "delete"     = deleteFile pPacket (mkPath p)

    -- Subscription commands
    d ["o"]      "sub"        = addSub env AllObjectsSub
    d ["o", oid] "sub"        = addSub env (ObjectSub (OId oid))
    d ["u"]      "sub"        = addSub env AllUsersSub
    d ("f":p)    "sub"        = addSub env (FileSub $ mkPath p)

    d ["o"]      "unsub"      = unSub env AllObjectsSub
    d ["o", oid] "unsub"      = unSub env (ObjectSub (OId oid))
    d ["u"]      "unsub"      = unSub env AllUsersSub
    d ("f":p)    "unsub"      = unSub env (FileSub $ mkPath p)

    d _ _                     = return $ errorResult pPacket "No such endpoint"


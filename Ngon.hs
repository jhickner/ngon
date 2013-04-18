{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WebSockets as WWS
import Network.Wai.Application.Static
import qualified Network.WebSockets as WS
import Network.HTTP.Types
import Network.Wai.Parse

import Data.Conduit (runResourceT, ($$))
import Data.Conduit.List (consume)

import System.FilePath
import System.Directory

import System.Posix.Signals hiding (Handler)

import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)

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
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Attoparsec as PB
import qualified Data.Aeson as A
import Data.Aeson (encode)

import Data.Maybe (fromJust)
import Data.Monoid

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
runWebServer env =
    W.runSettings W.defaultSettings
      { W.settingsPort = 8000
      , W.settingsIntercept = WWS.intercept (wsAPI env)
      } $ webApp env

webApp :: ServerEnv -> Application
webApp env req = case pathInfo req of
    ("api"  :eps) -> httpAPI eps env req
    ("files":eps) -> handleFiles eps env req 
    _             -> staticApp (defaultFileServerSettings "static") req

writeJSON :: (A.ToJSON a) => a -> Response
writeJSON =
    responseLBS ok200 [("Content-Type", "application/json")] . A.encode

rawRequestBody :: Request -> IO B.ByteString
rawRequestBody req = mconcat <$> runResourceT (requestBody req $$ consume)

-------------------------------------------------------------------------------
-- HTTP API

httpAPI :: [Text] -> ServerEnv -> Application
httpAPI endpoint env req = do
    packet <- liftIO getPacket
    rpacket <- (fromJust <$>) . liftIO $ runPacket $ mkPacketEnv env httpClient packet
    return $ writeJSON $ mplus (pError rpacket) (pPayload rpacket)
  where
    getPacket = do
        payload <- parsePayload req
        return $ Packet Nothing endpoint (parseAction req) payload Nothing

parsePayload :: Request -> IO (Maybe A.Value)
parsePayload req = do
    let mp = join . lookup "p" $ queryString req
    case mp of
      Just x  -> return $ decodeStrictValue x
      Nothing -> do
        body <- rawRequestBody req
        return $ if B.null body
                 then Nothing
                 else case decodeStrictValue body of
                        -- try again with quoted input
                        Nothing -> decodeStrictValue $ "\"" <> body <> "\""
                        x       -> x

parseAction :: Request -> Text
parseAction req = do
    let ma = join . lookup "a" $ queryString req
    case ma of
      Just x  -> toAction x
      Nothing ->
         case toAction $ requestMethod req of
           "post"   -> "set"
           m        -> m
  where toAction = T.toLower . decodeUtf8

-------------------------------------------------------------------------------
-- Files

handleFiles :: [Text] -> ServerEnv -> Application
handleFiles endpoint env req = case requestMethod req of
    "GET"  -> staticApp (defaultFileServerSettings ".") req
    "POST" -> handleUpload
    _      -> return $ responseLBS status404 [] BL.empty
  where
    handleUpload = do
        fs <- getFiles
        unless (null fs) $ liftIO $ do
            (rel, full) <- getPaths
            mkDirP full
            forM_ fs $ save full rel
        return $ responseLBS ok200 [] BL.empty
    getFiles = do
        (_, fs) <- parseRequestBody lbsBackEnd req 
        return [ (B.unpack (fileName f), fileContent f) | (_, f) <- fs ]
    getPaths = do
        pwd <- liftIO getCurrentDirectory
        let rel  = joinPath . map T.unpack $ safeEndpoint
            full = pwd </> fileRoot </> rel
        return (rel, full)
    safeEndpoint = if ".." `elem` endpoint then [] else endpoint
    save full rel (fn, bs) = do
        BL.writeFile (full </> fn) bs
        sendNotifications env (packet $ rel </> fn) [FileSub rel]
    mkDirP    = createDirectoryIfMissing True
    packet fp = Packet Nothing (fpToEndpoint fp) "create" Nothing Nothing

-------------------------------------------------------------------------------
-- WebSocket API

wsAPI :: ServerEnv -> WS.Request -> WS ()
wsAPI env rq = do
    WS.acceptRequest rq
    flip WS.catchWsError (const $ return ()) $ do
        sink <- WS.getSink
        client <- webSocketDecoder $ runConnectPacket env (WebSocketClient sink)
        flip WS.catchWsError (handler client) $
            webSocketDecoder $ \p -> do
                mrpacket <- runPacket $ mkPacketEnv env client p
                maybe (return ()) (WS.sendSink sink . WS.textData . encode) mrpacket
                return Nothing
  where
    handler client _e = liftIO $ disconnectUser (cUId client) env

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

    -- User commands
    d ["u"]      "get"        = okResult pPacket <$> getUsers env

    -- Message commands
    d ["m", uid] _            = msgUser (UId uid) env
    d ["m"]      _            = msgAllUsers env

    d _ _                     = return $ errorResult pPacket "No such endpoint"


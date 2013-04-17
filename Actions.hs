{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Actions where

import System.FilePath
import System.Directory
import Control.Exception

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad

import qualified Network.WebSockets as WS
import Network.Socket.ByteString.Lazy (sendAll)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import qualified Data.IxSet as Ix
import Data.IxSet ((@+), (@=))

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)

import Types


-------------------------------------------------------------------------------
-- Result combinators

errorResult :: Packet -> Text -> Result
errorResult p t = Error $ p { pError = Just $ A.String t }

ioErrorResult :: Monad m => Packet -> IOException -> m Result
ioErrorResult p e = return . Error $ addE p (T.pack $ show e)

maybeResult :: (A.ToJSON a) => Packet -> Text -> IO (Maybe a) -> IO Result
maybeResult p err m = do
  v <- m
  return $ maybe (errorResult p err) (okResult p) v

okResult :: (A.ToJSON a) => Packet -> a -> Result
okResult p v = OK NoNotifications $ addP p v

ok :: Packet -> Result
ok = OK NoNotifications

addP :: A.ToJSON a => Packet -> a -> Packet
addP p v = p { pPayload = Just $ A.toJSON v } 

addE :: Packet -> Text -> Packet
addE p err = p { pError = Just $ A.String err }

ioE :: Monad m => (IOException -> m a) -> IOException -> m a
ioE f = f

ioEUnit :: Monad m => IOException -> m ()
ioEUnit = ioE . const $ return ()


-------------------------------------------------------------------------------
-- Subscriptions

notify :: PacketEnv -> Result -> IO ()
notify env@PacketEnv{..} res =
    case res of
      Error{} -> return ()
      (OK n p) -> case n of
        ObjectCreated _    -> send' p [AllObjectsSub]
        ObjectUpdated oid  -> send' p [AllObjectsSub, ObjectSub oid]
        ObjectDeleted oid  -> send' p [AllObjectsSub, ObjectSub oid]
        ObjectLocked oid   -> send' p [AllObjectsSub, ObjectSub oid]
        ObjectUnlocked oid -> send' p [AllObjectsSub, ObjectSub oid]
        FileDeleted fp     -> send' p [FileSub fp]
        _ -> return ()
  where
    send' p = sendNotifications (mkServerEnv env) (p { pId = Nothing })

sendNotifications :: ServerEnv -> Packet -> [SubType] -> IO ()
sendNotifications env@ServerEnv{..} p sts = do
    clients <- getSubs
    mapM_ (\c -> sendPacket env c p) clients
  where
    getSubs = do
        subs' <- takeMVar sSubs
        umap' <- takeMVar sUsers
        putMVar sSubs subs'
        putMVar sUsers umap'
        return $ mapMaybe ((`M.lookup` umap') . sUId) . Ix.toList $ subs' @+ sts

sendPacket :: ServerEnv -> Client -> Packet -> IO ()
sendPacket env client p = case client of
    (Client uid (SocketClient s)) -> 
      wrap uid $ sendAll s . A.encode $ p
    (Client uid (WebSocketClient s)) -> 
      wrap uid $ WS.sendSink s . WS.textData . A.encode $ p
    _ -> return ()
  where
    wrap uid = handle (ioE $ const $ disconnectUser uid env)


sendHandlePacket :: ClientHandle -> Packet -> IO ()
sendHandlePacket ch p = case ch of
    (SocketClient s) -> handle ioEUnit $ sendAll s . A.encode $ p
    (WebSocketClient s) -> handle ioEUnit $ WS.sendSink s . WS.textData . A.encode $ p
    _ -> return ()

-------------------------------------------------------------------------------
-- Subscriptions

-- currently subs to objects that don't exist are allowed, and subs aren't
-- deleted when an object is deleted
addSub :: PacketEnv -> SubType -> IO Result
addSub env@PacketEnv{..} st = 
    case cHandle pClient of
        HTTPClient -> return $ errorResult pPacket "HTTP clients can't subscribe"
        _          -> do
            sendInitialUpdate env st
            modifyMVar_ pSubs $ \s -> return $
                Ix.insert entry (Ix.delete entry s) -- prevent dups
            return $ ok pPacket
  where
    entry = SubEntry (cUId pClient) st

unSub :: PacketEnv -> SubType -> IO Result
unSub PacketEnv{..} st = do
    modifyMVar_ pSubs $ \s -> return $
        Ix.delete (SubEntry (cUId pClient) st) s
    return $ ok pPacket

sendInitialUpdate :: PacketEnv -> SubType -> IO ()
sendInitialUpdate env@PacketEnv{..} st =
    case st of
      AllUsersSub -> do
        uids <- getUsers env
        forM_ uids $ \u -> sendPacket senv pClient $ 
            Packet Nothing ["u", getUId u] "connect" Nothing Nothing
      AllObjectsSub -> do
        objs <- getObjects env
        mapM_ sendObj objs
      ObjectSub oid -> do
        obj <- getObject oid env
        maybe (return ()) sendObj obj
      FileSub fp -> do
        files <- listFiles fp
        forM_ files $ \f -> sendPacket senv pClient $
            Packet Nothing (fpToEndpoint $ fp </> f) "create" Nothing Nothing
  where
    senv = mkServerEnv env
    sendObj o = case M.lookup "id" o of
        Just (A.String oid) -> sendPacket senv pClient $
            Packet Nothing ["o",oid] "create" (Just $ A.Object o) Nothing
        _   -> return ()

-------------------------------------------------------------------------------
-- Object Locks

withLockedObject :: UId -> OId
                 -> PacketEnv
                 -> (A.Object -> ObjectMap -> IO (ObjectMap, Result))
                 -> IO Result
withLockedObject uid oid env@PacketEnv{..} f = 
    withObject oid env $ \o m -> do
        l <- takeMVar pLocks
        res <- case Ix.getOne (l @= oid) of
            Just (Lock uid' _) | uid' == uid -> f o m
            Nothing -> f o m
            Just _  -> return (m, errorResult pPacket "Object is locked")
        void $ putMVar pLocks l
        return res

lockObject :: UId -> OId -> PacketEnv -> IO Result
lockObject uid oid env@PacketEnv{..} = 
    withObject oid env $ \_ m ->
        modifyMVar pLocks $ \l ->
          return $ case Ix.getOne (l @= oid) of
            Just (Lock uid' _) | uid' == uid -> 
              (l, (m, okResult pPacket uid))
            Nothing -> (Ix.insert (Lock uid oid) l, 
              (m, OK (ObjectLocked oid) (addP pPacket uid)))
            Just _  -> (l, (m, errorResult pPacket "Object is already locked"))

unlockObject :: UId -> OId -> PacketEnv -> IO Result
unlockObject uid oid env@PacketEnv{..} = 
    withObject oid env $ \_ m ->
        modifyMVar pLocks $ \l ->
          return $ case Ix.getOne (l @= oid) of
            Just (Lock uid' _) | uid' == uid -> 
              (Ix.delete (Lock uid oid) l, 
                (m, OK (ObjectUnlocked oid) pPacket))
            Nothing -> (l, (m, errorResult pPacket "Object is already unlocked"))
            Just _  -> (l, (m, errorResult pPacket "Object locked by another user"))

-------------------------------------------------------------------------------
-- Objects

withObject :: OId 
           -> PacketEnv
           -> (A.Object -> ObjectMap -> IO (ObjectMap, Result))
           -> IO Result
withObject oid env f =
    modifyMVar (pObjects env) $ \m -> 
      case M.lookup oid m of
        Nothing -> return (m, errorResult (pPacket env) "No such object id")
        Just o  -> f o m

createObject :: OId -> Maybe A.Value -> PacketEnv -> IO Result
createObject oid (Just (A.Object o)) PacketEnv{..} =
    modifyMVar pObjects $ \m ->
        return $ case M.lookup oid m of
          Nothing -> (M.insert oid o' m, OK (ObjectCreated oid) (addP pPacket o'))
          Just _  -> (m, errorResult pPacket "That object id already exists")
  where o' = M.insert "id" (A.toJSON oid) o -- insert oid as "id" prop
createObject _ _ env = return $ 
    errorResult (pPacket env) "The initial value must be an object"
  
getObject :: OId -> PacketEnv -> IO (Maybe A.Object)
getObject oid env = M.lookup oid <$> readMVar (pObjects env)

getObjects :: PacketEnv -> IO [A.Object]
getObjects env = M.elems <$> readMVar (pObjects env)

getObjectProp :: OId -> Text -> PacketEnv -> IO Result
getObjectProp oid prop env@PacketEnv{..} =
    withObject oid env $ \o m -> return $
      case M.lookup prop o of
        Nothing -> (m, errorResult pPacket "No such key in object")
        Just v  -> (m, okResult pPacket v)

incObject :: UId -> OId -> Maybe A.Value -> PacketEnv -> IO Result
incObject uid oid (Just (A.Object o)) env@PacketEnv{..} =
    withLockedObject uid oid env $ \o' m -> return $
        case updates o' of
          Nothing  -> (m, errorResult pPacket "Invalid increment")
          Just kvs -> let new = foldl' insert o' kvs in
              (M.insert oid new m, OK (ObjectUpdated oid) 
                  -- broadcast as a set to the incremented value
                  (pPacket { pAction = "set"
                           , pPayload = Just (A.toJSON $ M.fromList kvs)
                           }))
  where
    updates m = mapM (inc m) $ M.toList o
    inc m (k,v) = (,) k <$> join (flip incValue v <$> M.lookup k m)
    insert m (k,v) = M.insert k v m
incObject _ _ _ env = return $ errorResult (pPacket env) "The value must be an object"

incObjectProp :: UId -> OId -> Text -> Maybe A.Value 
              -> PacketEnv -> IO Result
incObjectProp uid oid prop (Just v) env@PacketEnv{..} =
    withLockedObject uid oid env $ \o m -> return $
      case M.lookup prop o of
          Nothing -> (m, errorResult pPacket "No such key in object")
          Just v' -> case incValue v' v of
            Nothing  -> (m, errorResult pPacket "Invalid increment")
            Just v'' -> (M.insert oid (M.insert prop v'' o) m, 
                         OK (ObjectUpdated oid) 
                         -- broadcast as a set to the incremented value
                         (pPacket { pAction = "set"
                                  , pEndpoint = ["o", getOId oid]
                                  , pPayload = Just $ A.object [prop .= v'']
                                  }))
incObjectProp _ _ _ _ env = return $ errorResult (pPacket env) "Nothing to inc"

mergeObject :: UId -> OId -> Maybe A.Value -> PacketEnv -> IO Result
mergeObject uid oid (Just (A.Object o)) env@PacketEnv{..} =
    withLockedObject uid oid env $ \o' m -> let new = o <> o' in
      return (M.insert oid new m, OK (ObjectUpdated oid) (addP pPacket o))
mergeObject _ _ _ env = return $ errorResult (pPacket env) "The value must be an object"

setObjectProp :: UId -> OId -> Text -> Maybe A.Value -> PacketEnv -> IO Result
setObjectProp uid oid prop (Just v) env@PacketEnv{..} =
    withLockedObject uid oid env $ \o m -> return
        (M.insert oid (M.insert prop v o) m, 
          OK (ObjectUpdated oid) 
          -- drop property from endpoint
          (pPacket { pEndpoint = ["o", getOId oid]
                   , pPayload = Just $ A.object [prop .= v] 
                   }))
setObjectProp _ _ _ _ env = return $ errorResult (pPacket env) "Nothing to set"

deleteObject :: UId -> OId -> PacketEnv -> IO Result
deleteObject uid oid env@PacketEnv{..} =
    withLockedObject uid oid env $ \_ m -> return
        (M.delete oid m, OK (ObjectDeleted oid) pPacket)

-- | attempt to combine two A.Values
incValue :: A.Value -> A.Value -> Maybe A.Value
incValue (A.String a) (A.String b) = Just . A.String $ a <> b
incValue (A.Array a)  (A.Array b)  = Just . A.Array  $ a <> b
incValue (A.Array a)  b            = Just . A.Array  $ a <> V.fromList [b]
incValue (A.Number a) (A.Number b) = Just . A.Number $ a +  b
incValue _ _ = Nothing

-------------------------------------------------------------------------------
-- Files

fileRoot = "files"

fpToEndpoint = map T.pack . ("f":) . splitDirectories 

mkPath :: [EndpointComponent] -> FilePath
mkPath = joinPath . map T.unpack

listFiles :: FilePath -> IO [String]
listFiles fp = handle (ioE $ const $ return []) $ listFiles' (fileRoot </> fp)
  where 
    listFiles' path' = 
        getDirectoryContents path' >>= filterM (doesFileExist . combine path')

deleteFile :: Packet -> FilePath -> IO Result
deleteFile p fp = handle (ioErrorResult p) $ do
    removeFile (fileRoot </> fp)
    return $ OK (FileDeleted rel) p
  where rel = case splitDirectories fp of
                      [_] -> ""
                      fs  -> joinPath $ init fs


-------------------------------------------------------------------------------
-- USERS

connectUser :: Client -> MVar UserMap -> IO Bool
connectUser client@Client{..} umap =
    modifyMVar umap $ \m ->
      return $ case M.lookup cUId m of
        Nothing -> (M.insert cUId client m, True)
        Just _  -> (m, False) 

disconnectUser :: UId -> ServerEnv -> IO ()
disconnectUser uid env@ServerEnv{..} = do
    putStrLn $ "[" ++ T.unpack (getUId uid) ++ "] - disconnected" 
    sendNotifications env
        (Packet Nothing ["u", getUId uid] "disconnect" Nothing Nothing)
        [AllUsersSub]
    modifyMVar_ sUsers $ return . M.delete uid
    modifyMVar_ sSubs $ \s -> return $
      foldl' (flip Ix.delete) s (Ix.toList $ Ix.getEQ uid s)
    modifyMVar_ sLocks $ \l -> return $
      foldl' (flip Ix.delete) l (Ix.toList $ Ix.getEQ uid l)

getUsers :: PacketEnv -> IO [UId]
getUsers env = M.keys <$> readMVar (pUsers env)

getClients :: PacketEnv -> IO [Client]
getClients env = M.elems <$> readMVar (pUsers env)

msgUser :: UId -> PacketEnv -> IO Result
msgUser uid env@PacketEnv{..} = do
    mclient <- M.lookup uid <$> readMVar pUsers
    case mclient of
      Nothing                    -> return $ errorResult pPacket "No such user id"
      Just (Client _ HTTPClient) -> return $ errorResult pPacket "Can't message HTTP clients"
      Just client                -> do
          sendPacket (mkServerEnv env) client p
          return $ ok pPacket
  where
    p = pPacket { pId = Nothing, pEndpoint = ["u", getUId (cUId pClient)] }

msgAllUsers :: PacketEnv -> IO Result
msgAllUsers env@PacketEnv{..} = do
    getClients env >>= mapM_ (\c -> sendPacket senv c p) 
    return $ ok pPacket
  where
    senv = mkServerEnv env
    p = pPacket { pId = Nothing, pEndpoint = ["u", getUId (cUId pClient)] }


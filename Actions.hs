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
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import qualified Data.IxSet as Ix
import Data.IxSet ((@+), (@=))

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import Types

true :: A.Value
true = A.Bool True

errorResult :: Text -> Result
errorResult = Error . A.String

ioErrorResult :: Monad m => IOException -> m Result
ioErrorResult = return . Error . A.String . T.pack . show

ioE :: Monad m => m a -> IOException -> m a
ioE f _ = f 

maybeResult :: (A.ToJSON a) => Text -> IO (Maybe a) -> IO Result
maybeResult err m = do
  r <- m
  return $ case r of
    Nothing -> errorResult err
    Just x  -> OK NoNotifications (A.toJSON x)

okResult :: (A.ToJSON a) =>  a -> Result
okResult = OK NoNotifications . A.toJSON

addError :: Text -> Packet -> Packet
addError err p = p { pError = Just $ A.String err }

-------------------------------------------------------------------------------
-- Subscriptions

notify :: PacketEnv -> Result -> IO ()
notify PacketEnv{..} res =
    case res of
      Error{} -> return ()
      (OK n _) -> case n of
        ObjectCreated _   -> send' [AllObjectsSub]
        ObjectUpdated oid -> send' [AllObjectsSub, ObjectSub oid]
        ObjectDeleted oid -> send' [AllObjectsSub, ObjectSub oid]
        FileDeleted fp    -> send' [FileSub $ takeDirectory fp]
        _ -> return ()
  where
    send' = sendNotifications pSubs pUsers (pPacket { pId = Nothing })

sendNotifications :: MVar SubSet -> MVar UserMap -> Packet -> [SubType] -> IO ()
sendNotifications subs umap p sts = do
    clients <- getSubs
    mapM_ (`sendPacket` p) clients
  where
    getSubs = do
        subs' <- takeMVar subs
        umap' <- takeMVar umap
        putMVar subs subs'
        putMVar umap umap'
        return $ mapMaybe ((`M.lookup` umap') . sUId) . Ix.toList $ subs' @+ sts

sendPacket :: Client -> Packet -> IO ()
sendPacket (Client _ (SocketClient s)) p = sendAll s . A.encode $ p
sendPacket (Client _ (WebSocketClient s)) p = 
    WS.sendSink s . WS.textData $ A.encode p
sendPacket _ _ = return ()


-------------------------------------------------------------------------------
-- Subscriptions

-- currently subs to objects that don't exist are allowed, and subs aren't
-- deleted when an object is deleted
addSub :: PacketEnv -> SubType -> IO Result
addSub env@PacketEnv{..} st = 
    case cHandle pClient of
        HTTPClient -> return $ errorResult "HTTP clients can't subscribe"
        _          -> do
            sendInitialUpdate env st
            modifyMVar_ pSubs $ \s -> return $
                Ix.insert entry (Ix.delete entry s) -- prevent dups
            return $ OK NoNotifications true
  where
    entry = SubEntry (cUId pClient) st

unSub :: PacketEnv -> SubType -> IO Result
unSub PacketEnv{..} st = do
    modifyMVar_ pSubs $ \s -> return $
        Ix.delete (SubEntry (cUId pClient) st) s
    return $ OK NoNotifications true

sendInitialUpdate :: PacketEnv -> SubType -> IO ()
sendInitialUpdate env@PacketEnv{..} st =
    case st of
      AllUsersSub -> do
        uids <- getUsers env
        forM_ uids $ \u -> sendPacket pClient $ 
            Packet Nothing ["u", getUId u] "connect" Nothing Nothing
      AllObjectsSub -> do
        objs <- getObjects env
        mapM_ sendObj objs
      ObjectSub oid -> do
        obj <- getObject oid env
        maybe (return ()) sendObj obj
      FileSub fp -> do
        files <- listFiles fp
        forM_ files $ \f -> sendPacket pClient $
            Packet Nothing (fpToEndpoint $ fp </> f) "create" Nothing Nothing
  where
    sendObj o = case M.lookup "id" o of
        Just (A.String oid) -> sendPacket pClient $
            Packet Nothing ["o",oid] "create" (Just $ A.Object o) Nothing
        _   -> return ()

-------------------------------------------------------------------------------
-- Object Locks

withLockedObject :: UId -> OId
                 -> MVar ObjectMap
                 -> MVar LockSet
                 -> (A.Object -> ObjectMap -> IO (ObjectMap, Result))
                 -> IO Result
withLockedObject uid oid omap locks f = 
    withObject oid omap $ \o m -> do
        l <- takeMVar locks
        res <- case Ix.getOne (l @= oid) of
            Just (Lock uid' _) | uid' == uid -> f o m
            Nothing -> f o m
            Just _  -> return (m, errorResult "Object is locked")
        void $ putMVar locks l
        return res

lockObject :: UId -> OId -> PacketEnv -> IO Result
lockObject uid oid PacketEnv{..} = 
    withObject oid pObjects $ \_ m ->
        modifyMVar pLocks $ \l ->
          return $ case Ix.getOne (l @= oid) of
            Just (Lock uid' _) | uid' == uid -> (l, (m, ok))
            Nothing -> (Ix.insert (Lock uid oid) l, (m, ok))
            Just _  -> (l, (m, errorResult "Object is already locked"))
  where
    ok = OK NoNotifications true

unlockObject :: UId -> OId -> PacketEnv -> IO Result
unlockObject uid oid PacketEnv{..} = 
    withObject oid pObjects $ \_ m -> do
        modifyMVar_ pLocks $ \l -> return $ Ix.delete (Lock uid oid) l
        return (m, OK NoNotifications true)

-------------------------------------------------------------------------------
-- Objects

withObject :: OId 
           -> MVar ObjectMap 
           -> (A.Object -> ObjectMap -> IO (ObjectMap, Result))
           -> IO Result
withObject oid omap f =
    modifyMVar omap $ \m -> 
      case M.lookup oid m of
        Nothing -> return (m, errorResult "No such object id")
        Just o  -> f o m

createObject :: OId -> Maybe A.Value -> PacketEnv -> IO Result
createObject oid (Just (A.Object o)) env =
    modifyMVar (pObjects env) $ \m ->
        return $ case M.lookup oid m of
          Nothing -> (M.insert oid o' m, OK (ObjectCreated oid) (A.toJSON o'))
          Just _  -> (m, errorResult "That object id already exists")
  where o' = M.insert "id" (A.toJSON oid) o -- insert oid as "id" prop
createObject _ _ _ = return $ errorResult "The initial value must be an object"
  
getObject :: OId -> PacketEnv -> IO (Maybe A.Object)
getObject oid env = M.lookup oid <$> readMVar (pObjects env)

getObjects :: PacketEnv -> IO [A.Object]
getObjects env = M.elems <$> readMVar (pObjects env)

getObjectProp :: OId -> Text -> PacketEnv -> IO Result
getObjectProp oid prop env =
    withObject oid (pObjects env) $ \o m -> return $
      case M.lookup prop o of
        Nothing -> (m, errorResult "No such key in object")
        Just v  -> (m, OK NoNotifications (A.toJSON v))

incObjectProp :: UId -> OId -> Text -> Maybe A.Value 
              -> PacketEnv -> IO Result
incObjectProp uid oid prop (Just v) PacketEnv{..} =
    withLockedObject uid oid pObjects pLocks $ \o m -> return $
      case M.lookup prop o of
          Nothing -> (m, errorResult "No such key in object")
          Just v' -> case incValue v' v of
            Nothing  -> (m, errorResult "Invalid increment")
            Just v'' -> (M.insert oid (M.insert prop v'' o) m, 
                         OK (ObjectUpdated oid) v'')
incObjectProp _ _ _ _ _ = return $ errorResult "Nothing to inc"

setObjectProp :: UId -> OId -> Text -> Maybe A.Value -> PacketEnv -> IO Result
setObjectProp uid oid prop (Just v) PacketEnv{..} =
    withLockedObject uid oid pObjects pLocks $ \o m -> return
        (M.insert oid (M.insert prop v o) m, OK (ObjectUpdated oid) v)
setObjectProp _ _ _ _ _ = return $ errorResult "Nothing to set"

deleteObject :: UId -> OId -> PacketEnv -> IO Result
deleteObject uid oid PacketEnv{..} =
    withLockedObject uid oid pObjects pLocks $ \_ m -> return
        (M.delete oid m, OK (ObjectDeleted oid) true)

mergeObject :: UId -> OId -> Maybe A.Value -> PacketEnv -> IO Result
mergeObject uid oid (Just (A.Object o)) PacketEnv{..} =
    withLockedObject uid oid pObjects pLocks $ \o' m -> let new = o <> o' in
      return (M.insert oid new m, OK (ObjectUpdated oid) (A.toJSON new))
mergeObject _ _ _ _ = return $ errorResult "The value must be an object"

incObject :: UId -> OId -> Maybe A.Value -> PacketEnv -> IO Result
incObject uid oid (Just (A.Object o)) PacketEnv{..} =
    withLockedObject uid oid pObjects pLocks $ \o' m -> return $
        case updates o' of
          Nothing  -> (m, errorResult "Invalid increment")
          Just kvs -> let new = foldl' insert o' kvs in
              (M.insert oid new m, OK (ObjectUpdated oid) (A.toJSON $ M.fromList kvs))
  where
    updates m = mapM (inc m) $ M.toList o
    inc m (k,v) = (,) k <$> join (flip incValue v <$> M.lookup k m)
    insert m (k,v) = M.insert k v m
incObject _ _ _ _ = return $ errorResult "The value must be an object"

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
listFiles fp = handle (ioE $ return []) $ listFiles' (fileRoot </> fp)
  where 
    listFiles' path' = 
        getDirectoryContents path' >>= filterM (doesFileExist . combine path')

deleteFile :: FilePath -> IO Result
deleteFile fp = handle ioErrorResult $ do
    removeFile (fileRoot </> fp)
    return $ OK (FileDeleted fp) true 


-------------------------------------------------------------------------------
-- USERS

connectUser :: Client -> MVar UserMap -> IO Bool
connectUser client@Client{..} umap =
    modifyMVar umap $ \m ->
      return $ case M.lookup cUId m of
        Nothing -> (M.insert cUId client m, True)
        Just _  -> (m, False) 

disconnectUser :: UId -> ServerEnv -> IO ()
disconnectUser uid ServerEnv{..} = do
    sendNotifications sSubs sUsers 
        (Packet Nothing ["u", getUId uid] "disconnect" Nothing Nothing)
        [AllUsersSub]
    modifyMVar_ sUsers $ return . M.delete uid
    modifyMVar_ sSubs $ \s -> return $
      foldl' (flip Ix.delete) s (Ix.toList $ Ix.getEQ uid s)
    modifyMVar_ sLocks $ \l -> return $
      foldl' (flip Ix.delete) l (Ix.toList $ Ix.getEQ uid l)

getUsers :: PacketEnv -> IO [UId]
getUsers env = M.keys <$> readMVar (pUsers env)

msgUser :: UId -> PacketEnv -> IO Result
msgUser uid PacketEnv{..} = do
    mclient <- M.lookup uid <$> readMVar pUsers
    case mclient of
      Nothing                    -> return $ errorResult "No such user id"
      Just (Client _ HTTPClient) -> return $ errorResult "Can't message HTTP clients"
      Just client                -> do
          sendPacket client (pPacket { pId = Nothing
                               , pEndpoint = ["u", getUId (cUId pClient)]
                               })
          return $ OK NoNotifications true 

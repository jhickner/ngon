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
import Data.IxSet ((@+))

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
    send' = sendNotifications pServerEnv (pPacket { pId = Nothing })

sendNotifications :: ServerEnv -> Packet -> [SubType] -> IO ()
sendNotifications env p sts = do
    clients <- getSubs env sts
    mapM_ (`sendPacket` p) clients

-- potential STM use here between subs and user map
getSubs :: ServerEnv -> [SubType] -> IO [Client]
getSubs ServerEnv{..} sts = do
    subs' <- readMVar sSubs
    umap' <- readMVar sUsers
    return $ mapMaybe ((`M.lookup` umap') . sUserId) . Ix.toList $ subs' @+ sts

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
            modifyMVar_ (sSubs pServerEnv) $ \s -> return $
                Ix.insert entry (Ix.delete entry s) -- prevent dups
            return $ OK NoNotifications true
  where
    entry = SubEntry (cUserId pClient) st

unSub :: PacketEnv -> SubType -> IO Result
unSub PacketEnv{..} st = do
    modifyMVar_ (sSubs pServerEnv) $ \s -> return $
        Ix.delete (SubEntry (cUserId pClient) st) s
    return $ OK NoNotifications true

sendInitialUpdate :: PacketEnv -> SubType -> IO ()
sendInitialUpdate PacketEnv{..} st =
    case st of
      AllUsersSub -> do
        uids <- getUsers sUsers
        forM_ uids $ \u -> sendPacket pClient $ 
            Packet Nothing ["u",u] "connect" Nothing Nothing
      AllObjectsSub -> do
        objs <- getObjects sObjects
        mapM_ sendObj objs
      ObjectSub oid -> do
        obj <- getObject oid sObjects
        maybe (return ()) sendObj obj
      FileSub fp -> do
        files <- listFiles fp
        forM_ files $ \f -> sendPacket pClient $
            Packet Nothing (fpToEndpoint $ fp </> f) "create" Nothing Nothing
  where
    ServerEnv{..} = pServerEnv
    sendObj o = case M.lookup "id" o of
        Just (A.String oid) -> sendPacket pClient $
            Packet Nothing ["o",oid] "create" (Just $ A.Object o) Nothing
        _   -> return ()

-------------------------------------------------------------------------------
-- Object Locks


-------------------------------------------------------------------------------
-- Objects

createObject :: ObjectId -> Maybe A.Value -> MVar ObjectMap -> IO Result
createObject oid (Just (A.Object o)) omap =
    modifyMVar omap $ \m ->
        return $ case M.lookup oid m of
          Nothing -> (M.insert oid o' m, OK (ObjectCreated oid) (A.toJSON o'))
          Just _  -> (m, errorResult "That object id already exists")
  where o' = M.insert "id" (A.toJSON oid) o -- insert oid as "id" prop
createObject _ _ _ = return $ errorResult "The initial value must be an object"
  
getObject :: ObjectId -> MVar ObjectMap -> IO (Maybe A.Object)
getObject oid omap = M.lookup oid <$> readMVar omap

getObjects :: MVar ObjectMap -> IO [A.Object]
getObjects omap = M.elems <$> readMVar omap

getObjectProp :: ObjectId -> Text -> MVar ObjectMap -> IO Result
getObjectProp oid prop omap = do
    obj <- M.lookup oid <$> readMVar omap
    return $ case obj of
      Nothing -> errorResult "No such object id"
      Just o  -> case M.lookup prop o of
        Nothing -> errorResult "No such key in object"
        Just v  -> OK NoNotifications (A.toJSON v) 

incObjectProp :: ObjectId -> Text -> Maybe A.Value -> MVar ObjectMap -> IO Result
incObjectProp oid prop (Just v) omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id")
        Just o' -> case M.lookup prop o' of
          Nothing -> (m, errorResult "No such key in object")
          Just v' -> case incValue v' v of
            Nothing  -> (m, errorResult "Invalid increment")
            Just v'' -> (M.insert oid (M.insert prop v'' o') m, 
                         OK (ObjectUpdated oid) v'')
incObjectProp _ _ _ _ = return $ errorResult "Nothing to inc"

setObjectProp :: ObjectId -> Text -> Maybe A.Value -> MVar ObjectMap -> IO Result
setObjectProp oid prop (Just v) omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id")
        Just o' -> (M.insert oid (M.insert prop v o') m, OK (ObjectUpdated oid) v)
setObjectProp _ _ _ _ = return $ errorResult "Nothing to set"

deleteObject :: ObjectId -> MVar ObjectMap -> IO Result
deleteObject oid omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id") 
        Just _  -> (M.delete oid m, OK (ObjectDeleted oid) true)

mergeObject :: ObjectId -> Maybe A.Value -> MVar ObjectMap -> IO Result
mergeObject oid (Just (A.Object o)) omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id")
        Just o' -> let new = o <> o' in
          (M.insert oid new m, OK (ObjectUpdated oid) (A.toJSON new))
mergeObject _ _ _ = return $ errorResult "The value must be an object"

incObject :: ObjectId -> Maybe A.Value -> MVar ObjectMap -> IO Result
incObject oid (Just (A.Object o)) omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id")
        Just o' -> 
          case updates o' of
            Nothing  -> (m, errorResult "Invalid increment")
            Just kvs -> let new = foldl' insert o' kvs in
                (M.insert oid new m, OK (ObjectUpdated oid) (A.toJSON new))
  where
    updates m = mapM (inc m) $ M.toList o
    inc m (k,v) = (,) k <$> join (flip incValue v <$> M.lookup k m)
    insert m (k,v) = M.insert k v m
incObject _ _ _ = return $ errorResult "The value must be an object"

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
      return $ case M.lookup cUserId m of
        Nothing -> (M.insert cUserId client m, True)
        Just _  -> (m, False) 

disconnectUser :: UserId -> ServerEnv -> IO ()
disconnectUser uid env@ServerEnv{..} = do
    sendNotifications env 
        (Packet Nothing ["u", uid] "disconnect" Nothing Nothing)
        [AllUsersSub]
    modifyMVar_ sUsers $ return . M.delete uid
    modifyMVar_ sSubs $ \s -> return $
      foldl' (flip Ix.delete) s (Ix.toList $ Ix.getEQ uid s)

getUsers :: MVar UserMap -> IO [Text]
getUsers umap = M.keys <$> readMVar umap

msgUser :: Packet -> UserId -> Client -> MVar UserMap -> IO Result
msgUser p uid (Client fuid _) umap = do
    mclient <- M.lookup uid <$> readMVar umap
    case mclient of
      Nothing                    -> return $ errorResult "No such user id"
      Just (Client _ HTTPClient) -> return $ errorResult "Can't message HTTP clients"
      Just client                -> do
          sendPacket client (p { pId = Nothing
                               , pEndpoint = ["u", fuid]
                               })
          return $ OK NoNotifications true 
      

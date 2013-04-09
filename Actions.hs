{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Actions where

import System.FilePath
import System.Directory
import Control.Exception

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad

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

import Debug.Trace

true :: A.Value
true = A.Bool True

errorResult :: Text -> Result
errorResult = Error . A.String

ioErrorResult :: Monad m => IOException -> m Result
ioErrorResult = return . Error . A.String . T.pack . show

-------------------------------------------------------------------------------
-- Subscriptions

-- file subs: FilePath
-- object subs: ObjectId
-- object subs: All Objects
-- user subs: All Users

tap x = traceShow x x

sendPacket :: Packet -> Client -> IO ()
sendPacket p (Client _ (SocketClient s)) = sendAll s . A.encode $ p
sendPacket _ _ = return ()

-- potential STM use here between subs and user map
notify :: PacketEnv -> Result -> IO ()
notify PacketEnv{..} res =
    case res of
      Error{} -> return ()
      (OK _ n) -> do
        subs' <- readMVar (sSubs pServerEnv)
        umap' <- readMVar (sUsers pServerEnv)
        mapM_ (sendPacket $ pPacket { pId = Nothing }) $ clients subs' umap' n
  where
    clients subs' umap' n = lookup' $ case n of
        ObjectCreated _   -> [AllObjectsSub]
        ObjectUpdated oid -> [AllObjectsSub, ObjectSub oid]
        _ -> []
      where 
        lookup' = mapMaybe ((`M.lookup` umap') . sUserId) . Ix.toList . (subs' @+) 


subObjects :: Client -> MVar SubSet -> IO Result
subObjects client subs = do
    modifyMVar_ subs $ \s -> return $
        Ix.insert (SubEntry (cUserId client) AllObjectsSub) s
    return $ OK true NoNotifications


-------------------------------------------------------------------------------
-- Objects

createObject :: ObjectId -> Maybe A.Value -> MVar ObjectMap -> IO Result
createObject oid (Just (A.Object o)) omap =
    modifyMVar omap $ \m ->
        return $ case M.lookup oid m of
          Nothing -> (M.insert oid o m, OK (A.toJSON o) $ ObjectCreated oid)
          Just _  -> (m, errorResult "That object id already exists")
createObject _ _ _ = return $ errorResult "The initial value must be an object"
  
getObject :: ObjectId -> MVar ObjectMap -> IO Result
getObject oid omap = do
    obj <- M.lookup oid <$> readMVar omap
    return $ case obj of
      Nothing -> errorResult "No such object id"
      Just o  -> OK (A.Object o) NoNotifications
     
getObjectProp :: ObjectId -> Text -> MVar ObjectMap -> IO Result
getObjectProp oid prop omap = do
    obj <- M.lookup oid <$> readMVar omap
    return $ case obj of
      Nothing -> errorResult "No such object id"
      Just o  -> case M.lookup prop o of
        Nothing -> errorResult "No such key in object"
        Just v  -> OK (A.toJSON v) NoNotifications

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
                         OK v'' $ ObjectUpdated oid)
incObjectProp _ _ _ _ = return $ errorResult "Nothing to inc"

setObjectProp :: ObjectId -> Text -> Maybe A.Value -> MVar ObjectMap -> IO Result
setObjectProp oid prop (Just v) omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id")
        Just o' -> (M.insert oid (M.insert prop v o') m, OK v $ ObjectUpdated oid)
setObjectProp _ _ _ _ = return $ errorResult "Nothing to set"
-- delete object
-- send deleted notification
-- delete object subs (should notifications handle this?)

deleteObject :: ObjectId -> MVar ObjectMap -> IO Result
deleteObject oid omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id") 
        Just _  -> (M.delete oid m, OK true $ ObjectDeleted oid)

mergeObject :: ObjectId -> Maybe A.Value -> MVar ObjectMap -> IO Result
mergeObject oid (Just (A.Object o)) omap =
    modifyMVar omap $ \m ->
      return $ case M.lookup oid m of
        Nothing -> (m, errorResult "No such object id")
        Just o' -> let new = o <> o' in
          (M.insert oid new m, OK (A.toJSON new) $ ObjectUpdated oid)
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
                (M.insert oid new m, OK (A.toJSON new) $ ObjectUpdated oid)
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

root = "files"

mkPath :: [EndpointComponent] -> FilePath
mkPath = joinPath . (root :) . map T.unpack

listFiles :: [EndpointComponent] -> IO Result
listFiles eps = handle ioErrorResult $ do
    files <- listFiles' $ mkPath eps
    return $ OK (A.toJSON files) NoNotifications
  where 
    listFiles' path' = 
        getDirectoryContents path' >>= filterM (doesFileExist . combine path')

deleteFile :: [EndpointComponent] -> IO Result
deleteFile eps = handle ioErrorResult $ do
    removeFile subpath
    return $ OK true (FileDeleted subpath)
  where 
    subpath = mkPath eps


-------------------------------------------------------------------------------
-- USERS

connectUser :: UserId -> Client -> MVar UserMap -> IO Result
connectUser _ (Client _ HTTPClient) _ = 
    return $ errorResult "HTTP clients do not need to connect."
connectUser uid client umap = do
    res <- modifyMVar umap $ \m ->
      return $ case M.lookup uid m of
        Nothing -> (M.insert uid client m, True)
        Just _  -> (m, False) 
    return $ if res
      then OK true (UserConnected uid)
      else errorResult "That user id is already in use."

disconnectUser :: UserId -> MVar UserMap -> IO Result
disconnectUser uid umap = do
    res <- modifyMVar umap $ \m ->
      return $ case M.lookup uid m of
        Nothing -> (m, False)
        Just _  -> (M.delete uid m, True) 
    return $ if res
      then OK true (UserDisconnected uid)
      else errorResult "No such user id"

getUsers :: MVar UserMap -> IO Result
getUsers umap = do
    uids <- M.keys <$> readMVar umap
    return $ OK (A.toJSON uids) NoNotifications

msgUser :: Packet -> UserId -> MVar UserMap -> IO Result
msgUser p uid umap = do
    mclient <- M.lookup uid <$> readMVar umap
    case mclient of
      Nothing                    -> return $ errorResult "No such user id."
      Just (Client _ HTTPClient) -> return $ errorResult "Can't message HTTP clients."
      Just client                -> do
          sendPacket (p { pId = Nothing }) client
          return $ OK true NoNotifications
      

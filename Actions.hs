{-# LANGUAGE OverloadedStrings #-}

module Actions where

import System.FilePath
import System.Directory
import Control.Exception

import Control.Concurrent.STM
import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Maybe (isJust)
import Types

true :: A.Value
true = A.Bool True

errorResult :: Text -> Result
errorResult = Error . A.String

ioErrorResult :: Monad m => IOException -> m Result
ioErrorResult = return . Error . A.String . T.pack . show


-------------------------------------------------------------------------------
-- Objects

objectExists :: ObjectId -> TVar ObjectMap -> STM Bool
objectExists oid omap = isJust . M.lookup oid <$> readTVar omap

createObject :: ObjectId -> Maybe A.Value -> TVar ObjectMap -> STM Result
createObject oid (Just (A.Object o)) omap = do
    exists <- objectExists oid omap
    if exists
      then return $ errorResult "That object id already exists"
      else do
          modifyTVar' omap (M.insert oid o)
          return $ OK true (ObjectCreated oid)
createObject _ _ _ = return $ errorResult "The initial value must be an object"
  
getObject :: ObjectId -> TVar ObjectMap -> STM Result
getObject oid omap = do
    obj <- M.lookup oid <$> readTVar omap
    return $ case obj of
      Nothing -> errorResult "No such object id"
      Just o  -> OK (A.Object o) NoNotifications
    
mergeObject :: ObjectId -> Maybe A.Value -> TVar ObjectMap -> STM Result
mergeObject oid (Just (A.Object o)) omap = do
    obj <- M.lookup oid <$> readTVar omap
    case obj of
      Nothing  -> return $ errorResult "No such object id"
      Just o' -> do
          modifyTVar' omap (M.insert oid $ o <> o')
          return $ OK true (ObjectUpdated oid)
setObject _ _ _ = return $ errorResult "The value must be an object"

incObject :: ObjectId -> Maybe A.Value -> TVar ObjectMap -> STM Result
incObject oid (Just (A.Object o)) omap = do
    obj <- M.lookup oid <$> readTVar omap
    case obj of
      Nothing  -> return $ errorResult "No such object id"
      Just o' ->
          case updates o' of
            Nothing  -> return $ errorResult "Invalid increment"
            Just kvs -> do
                modifyTVar' omap (M.insert oid $ foldl' insert o' kvs)
                return $ OK true (ObjectUpdated oid)
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

uidExists :: UserId -> TVar UserMap -> STM Bool
uidExists uid umap = isJust . M.lookup uid <$> readTVar umap

connectUser :: UserId -> Client -> TVar UserMap -> STM Result
connectUser _ HTTPClient _ = return $ errorResult "HTTP clients do not need to connect."
connectUser uid client umap = do
    exists <- uidExists uid umap
    if exists
      then return $ errorResult "That user id is already in use."
      else do
          modifyTVar' umap (M.insert uid client) 
          return $ OK true (UserConnected uid)

disconnectUser :: UserId -> TVar UserMap -> STM Result
disconnectUser uid umap = do
    exists <- uidExists uid umap
    if exists
      then do
          modifyTVar' umap $ M.delete uid
          return $ OK true $ UserDisconnected uid
      else return $ errorResult "No such user id."

getUsers :: TVar UserMap -> STM Result
getUsers umap = do
    uids <- M.keys <$> readTVar umap
    return $ OK (A.toJSON uids) NoNotifications

msgUser :: UserId -> Maybe A.Value -> TVar UserMap -> STM Result
msgUser _ Nothing _ = return $ errorResult "Nothing to send."
msgUser uid (Just v) umap = do
    client <- M.lookup uid <$> readTVar umap
    return $ case client of
      Nothing         -> errorResult "No such user id."
      Just HTTPClient -> errorResult "Can't message HTTP clients."
      Just _          -> OK true $ UserMessage uid v
      

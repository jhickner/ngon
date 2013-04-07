{-# LANGUAGE OverloadedStrings #-}

module Actions where

import Control.Concurrent.STM
import Control.Applicative

import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M

import Data.Maybe (isJust)
import Types

true :: A.Value
true = A.Bool True

errorResult :: Text -> Result
errorResult = Error . A.String

-------------------------------------------------------------------------------
-- USERS

uidExists :: UserId -> TVar UserMap -> STM Bool
uidExists uid umap = isJust . M.lookup uid <$> readTVar umap

connect :: UserId -> Client -> TVar UserMap -> STM Result
connect uid client umap = do
    exists <- uidExists uid umap
    if exists
      then return $ errorResult "That user id is already in use."
      else do
          modifyTVar' umap (M.insert uid client) 
          return $ OK true (UserConnected uid)

disconnect :: UserId -> TVar UserMap -> STM NotificationType
disconnect uid umap = do
    exists <- uidExists uid umap
    if exists
      then do
          modifyTVar' umap $ M.delete uid
          return $ UserDisconnected uid
      else return NoNotification

getUsers :: TVar UserMap -> STM Result
getUsers umap = do
    uids <- M.keys <$> readTVar umap
    return $ OK (A.toJSON uids) NoNotification

msgUser :: UserId -> Maybe A.Value -> TVar UserMap -> STM Result
msgUser _ Nothing _ = return $ errorResult "Nothing to send."
msgUser uid (Just v) umap = do
    client <- M.lookup uid <$> readTVar umap
    return $ case client of
      Nothing         -> errorResult "No such user id."
      Just HTTPClient -> errorResult "Can't message http clients."
      Just _          -> OK true $ UserMessage uid v
      

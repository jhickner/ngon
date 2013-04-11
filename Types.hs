{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar

import Network.Socket (Socket)
import Network.WebSockets (WebSockets, Sink, TextProtocol, Hybi00)

import Data.Data
import Data.IxSet
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Object, Value(..), object, ToJSON(..), FromJSON(..), (.=), (.:?), (.:))
-- import Data.Attoparsec.Text
import Data.Monoid
import Data.Maybe (isJust, fromMaybe)

type ObjectId = Text
type UserId   = Text

type UserMap = HashMap UserId Client
type ObjectMap = HashMap ObjectId Object
type SubSet = IxSet SubEntry

data ServerEnv = ServerEnv
  { sUsers   :: MVar UserMap
  , sObjects :: MVar ObjectMap
  , sLocks   :: MVar LockSet
  , sSubs    :: MVar SubSet
  }

-- ServerEnv plus current Client and Packet
data PacketEnv = PacketEnv 
  { pServerEnv :: ServerEnv
  , pClient    :: Client
  , pPacket    :: Packet
  }

type WS = WebSockets Hybi00

instance TextProtocol p => Show (Sink p) where
  show _ = "<Sink>"

data Client = Client
  { cUserId :: UserId
  , cHandle :: ClientHandle
  } deriving (Show, Eq)

data ClientHandle 
  = SocketClient Socket 
  | WebSocketClient (Sink Hybi00) 
  | HTTPClient
    deriving (Show, Eq)

instance ToJSON Client where
  toJSON (Client uid _) = String uid
  
httpClient = Client "http" HTTPClient

data Packet = Packet
    { pId       :: Maybe Integer
    , pEndpoint :: [EndpointComponent]
    , pAction   :: Text
    , pPayload  :: Maybe Value
    , pError    :: Maybe Value
    } deriving (Show, Eq)

hasId :: Packet -> Bool
hasId = isJust . pId

type EndpointComponent = Text

instance FromJSON Packet where
  parseJSON (Object o) = do
      id' <- o .:? "id"
      e   <- map T.strip . T.split (=='/') <$> o .: "e" 
      a   <- (T.toLower <$>) <$> o .:? "a"
      p   <- o .:? "p"
      err <- o .:? "err"
      let defaultAction = case p of
                            Nothing -> "get"
                            _       -> "set"
      return $ Packet id' e (fromMaybe defaultAction a) p err 
  parseJSON _ = mzero

instance ToJSON Packet where
  toJSON Packet{..} = object $ mconcat 
    [ maybeField "id" pId
    , ["e" .= T.intercalate "/" pEndpoint]
    , ["a" .= pAction]
    , maybeField "p" pPayload
    , maybeField "err" pError
    ]
    where
      maybeField _ Nothing     = mempty
      maybeField name (Just a) = [name .= a]

data Result 
  = OK Notifications Value
  | Error Value 
    deriving (Show, Eq)

data Notifications 
  = NoNotifications
  | ObjectUpdated ObjectId
  | ObjectCreated ObjectId
  | ObjectDeleted ObjectId
  | UserDisconnected UserId
  | UserConnected UserId
  | FileDeleted FilePath
    deriving (Show, Eq)


data SubType
  = FileSub FilePath
  | ObjectSub ObjectId
  | AllObjectsSub
  | AllUsersSub
    deriving (Show, Eq, Ord, Data, Typeable)

data SubEntry = SubEntry 
    { sUserId  :: UserId
    , sSubType :: SubType 
    }
    deriving (Show, Eq, Ord, Data, Typeable)

instance Indexable SubEntry where
    empty = ixSet [ ixFun $ \(SubEntry uid _) -> [uid]
                  , ixFun $ \(SubEntry _ st)  -> [st] 
                  ]

type LockSet = IxSet Lock

data Lock = Lock UserId ObjectId deriving (Show, Eq, Data, Typeable)

instance Indexable Lock where
    empty = ixSet [ ixFun $ \(Lock uid _) -> [uid]
                  , ixFun $ \(Lock _ oid) -> [oid]
                  ]

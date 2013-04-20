{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Ngon.Types where

import Control.Concurrent.MVar

import Network.Socket (Socket)
import Network.WebSockets (WebSockets, Sink, TextProtocol, Hybi00)

import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Data
import Data.IxSet
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Object, Value(..), ToJSON(..))

import Ngon.Packet

data OId = OId { getOId :: Text} deriving (Show, Eq, Ord, Data, Typeable)
data UId = UId { getUId :: Text} deriving (Show, Eq, Ord, Data, Typeable)

instance Hashable OId where hashWithSalt = hashUsing getOId
instance Hashable UId where hashWithSalt = hashUsing getUId

instance ToJSON OId where toJSON (OId oid) = String oid
instance ToJSON UId where toJSON (UId uid) = String uid

type UserMap   = HashMap UId Client
type ObjectMap = HashMap OId Object
type SubSet    = IxSet SubEntry
type LockSet   = IxSet Lock

data ServerEnv = ServerEnv
  { sUsers   :: MVar UserMap
  , sObjects :: MVar ObjectMap
  , sLocks   :: MVar LockSet
  , sSubs    :: MVar SubSet
  }

-- ServerEnv plus current Client and Packet
data PacketEnv = PacketEnv 
  { pServerEnv :: ServerEnv
  , pClient  :: Client
  , pPacket  :: Packet
  }

pUsers   = sUsers . pServerEnv
pObjects = sObjects . pServerEnv
pLocks   = sLocks . pServerEnv
pSubs    = sSubs . pServerEnv

type WS = WebSockets Hybi00

instance TextProtocol p => Show (Sink p) where
  show _ = "<Sink>"

data Lock = Lock UId OId deriving (Show, Eq, Ord, Data, Typeable)

instance Indexable Lock where
    empty = ixSet [ ixFun $ \(Lock uid _) -> [uid]
                  , ixFun $ \(Lock _ oid) -> [oid]
                  ]

data Client = Client
  { cUId    :: UId
  , cHandle :: ClientHandle
  } deriving Eq

instance Show Client where
  show (Client uid _) = T.unpack $ getUId uid

data ClientHandle 
  = SocketClient Socket 
  | WebSocketClient (Sink Hybi00) 
  | HTTPClient
    deriving (Show, Eq)

instance ToJSON Client where
  toJSON (Client (UId uid) _) = String uid
  
httpClient :: Client
httpClient = Client (UId "http") HTTPClient

isHTTP :: Client -> Bool
isHTTP client = cHandle client == HTTPClient 

data Result 
  = OK Notifications Packet
  | Error Packet
    deriving (Show, Eq)

data Notifications 
  = NoNotifications
  | ObjectUpdated OId
  | ObjectCreated OId
  | ObjectDeleted OId
  | ObjectLocked OId
  | ObjectUnlocked OId
  | UserDisconnected UId
  | UserConnected UId
  | FileDeleted FilePath
    deriving (Show, Eq)

data SubType
  = FileSub FilePath
  | ObjectSub OId
  | AllObjectsSub
  | AllUsersSub
    deriving (Show, Eq, Ord, Data, Typeable)

data SubEntry = SubEntry 
    { sUId  :: UId
    , sSubType :: SubType 
    }
    deriving (Show, Eq, Ord, Data, Typeable)

instance Indexable SubEntry where
    empty = ixSet [ ixFun $ \(SubEntry uid _) -> [uid]
                  , ixFun $ \(SubEntry _ st)  -> [st] 
                  ]

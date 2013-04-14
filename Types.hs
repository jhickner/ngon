{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar

import Network.Socket (Socket)
import Network.WebSockets (WebSockets, Sink, TextProtocol, Hybi00)

import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.Data
import Data.IxSet
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Object, Value(..), object, ToJSON(..), FromJSON(..), (.=), (.:?), (.:))

import Data.Monoid
import Data.Maybe (isJust, fromMaybe)

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
  { pUsers   :: MVar UserMap
  , pObjects :: MVar ObjectMap
  , pLocks   :: MVar LockSet
  , pSubs    :: MVar SubSet
  , pClient  :: Client
  , pPacket  :: Packet
  }

mkPacketEnv :: ServerEnv -> Client -> Packet -> PacketEnv
mkPacketEnv ServerEnv{..} = PacketEnv sUsers sObjects sLocks sSubs

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
  } deriving (Show, Eq)

data ClientHandle 
  = SocketClient Socket 
  | WebSocketClient (Sink Hybi00) 
  | HTTPClient
    deriving (Show, Eq)

instance ToJSON Client where
  toJSON (Client (UId uid) _) = String uid
  
httpClient = Client (UId "http") HTTPClient

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
      return $ Packet id' e (action p a) p err 
    where
      action p = fromMaybe (maybe "get" (const "set") p)
  parseJSON _ = mzero

instance ToJSON Packet where
  toJSON Packet{..} = object $ mconcat 
    [ maybeField "id" pId
    , ["e" .= T.intercalate "/" pEndpoint]
    -- trim set and get since they can inferred
    , if pAction `elem` ["set", "get"] then mempty else ["a" .= pAction]
    , maybeField "p" pPayload
    , maybeField "err" pError
    ]
    where
      maybeField _ Nothing     = mempty
      maybeField name (Just a) = [name .= a]

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

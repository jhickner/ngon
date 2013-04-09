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
import Data.Maybe (fromMaybe)

type ObjectId = Text
type UserId   = Text

type UserMap = HashMap UserId Client
type ObjectMap = HashMap ObjectId Object
type SubSet = IxSet SubEntry

data ServerEnv = ServerEnv
  { sUsers   :: MVar UserMap
  , sObjects :: MVar ObjectMap
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


type EndpointComponent = Text

instance FromJSON Packet where
  -- parseJSON (Object o) = Packet <$> o .:? "id"
                                -- <*> (map T.strip . T.split (=='/') <$> o .: "e")
                                -- <*> o .:? "p"
                                -- <*> o .:? "err"
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


{-
endpointComponents :: Parser [EndpointComponent]
endpointComponents = many1 $ char '/' *> component <|> component
  where component = Num <$> decimal <|> Str <$> takeWhile1 (/= '/') 
-}

isOKResult (OK _ _) = True
isOKResult _        = False

data Result 
  = OK Value Notifications 
  | Error Value 
    deriving (Show, Eq)

data Notifications 
  = NoNotifications
  | ObjectUpdated ObjectId
  | ObjectCreated ObjectId
  | ObjectDeleted ObjectId
  | UserMessage UserId Value
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

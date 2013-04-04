{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.STM

import Network.Socket (Socket)
import Network.WebSockets (Sink, TextProtocol, Hybi10)

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..), object, ToJSON(..), FromJSON(..), (.=), (.:?), (.:))
-- import Data.Attoparsec.Text
import Data.Monoid

type ObjectId = Text
type UserId   = Text

type UserMap = HashMap UserId Client

data Env = Env
  { eUsers :: TVar UserMap
  -- current packet
  -- current client
  }

type DispatchEnv = (Env, Client, Packet)

newtype NGON a = NGON 
  { runN :: ReaderT DispatchEnv IO a } 
  deriving (Monad, MonadIO, MonadReader DispatchEnv)

runNGON :: DispatchEnv -> NGON a -> IO a
runNGON denv k = runReaderT (runN k) denv

instance TextProtocol p => Show (Sink p) where
  show _ = "<Sink>"

data Client 
  = SocketClient Socket 
  | WebSocketClient (Sink Hybi10) 
  | HTTPClient
    deriving (Show, Eq)

data Packet = Packet
    { pId       :: Maybe Integer
    , pEndpoint :: [EndpointComponent]
    , pPayload  :: Maybe Value
    , pError    :: Maybe Value
    } deriving (Show, Eq)


type EndpointComponent = Text


instance FromJSON Packet where
  parseJSON (Object o) = Packet <$> o .:? "id"
                                <*> (map T.strip . T.split (=='/') <$> o .: "e")
                                <*> o .:? "p"
                                <*> o .:? "err"
  parseJSON _ = mzero

instance ToJSON Packet where
  toJSON Packet{..} = object $ mconcat 
    [ maybeField "id" pId
    , ["e" .= pEndpoint]
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

data Result = OK Value Notification | Error Value deriving (Show, Eq)

data Notification = NoNotification
                  | ObjectUpdated ObjectId
                  | UserUpdated UserId
                  deriving (Show, Eq)

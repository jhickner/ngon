{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Applicative
import Control.Monad

import Network.Socket (Socket)
import Network.WebSockets (Sink, TextProtocol, Hybi10)

import Data.Text (Text)
import Data.Aeson (Value(..), FromJSON(..), (.:?), (.:))
import Data.Attoparsec.Text


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
    } deriving Show

data EndpointComponent = Num Int | Str Text deriving (Show, Eq)

instance FromJSON Packet where
  -- parseJSON (Object o) = Packet <$> o .:? "id"
                                -- <*> (o .: "e" <|> o .:  "endpoint")
                                -- <*> (o .: "p" <|> o .:? "payload")
  parseJSON (Object o) = do
      id' <- o .:? "id"
      e <- o .: "e"
      p <- o .:? "p"
      case parseOnly endpointComponents e of
          Left _   -> mzero
          Right es -> return $ Packet id' es p

endpointComponents :: Parser [EndpointComponent]
endpointComponents = many1 $ char '/' *> component <|> component
  where component = Num <$> decimal <|> Str <$> takeWhile1 (/= '/') 


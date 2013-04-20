{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Ngon.Packet where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe

data Packet = Packet
    { pId       :: Maybe Integer
    , pEndpoint :: [EndpointComponent]
    , pAction   :: Text
    , pPayload  :: Maybe Value
    , pError    :: Maybe Value
    } deriving (Eq)

instance Show Packet where
  show = BL.unpack . encode

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

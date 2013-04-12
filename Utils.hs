module Utils where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as P
import qualified Data.Attoparsec as PB

import Debug.Trace

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

decodeStrictValue :: B.ByteString -> Maybe A.Value
decodeStrictValue bs = 
    case PB.parseOnly P.value bs of
      Left _  -> Nothing
      Right v -> Just v

-- | strict Aeson decoding
decodeStrict :: A.FromJSON a => B.ByteString -> Maybe a
decodeStrict bs = 
    case PB.parseOnly P.json' bs of
      Left _  -> Nothing
      Right v -> case A.fromJSON v of
                   A.Success a -> Just a
                   _           -> Nothing

{-
tap x = traceShow x x

decodeStrict :: A.FromJSON a => B.ByteString -> Maybe a
decodeStrict bs = 
    case PB.maybeResult $ PB.feed (PB.parse P.json' (tap bs)) B.empty of
      Just v -> case A.fromJSON v of
                   A.Success a -> Just a
                   _           -> Nothing
      _ -> Nothing
-}

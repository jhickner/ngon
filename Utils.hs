module Utils where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as P
import Data.Attoparsec

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

-- try to parse a Value, only succeeding if all input is consumed
decodeStrictValue :: B.ByteString -> Maybe A.Value
decodeStrictValue bs = 
    case parse P.value' bs of
      Fail{}    -> Nothing
      Partial _ -> Nothing
      Done t r  -> if B.null t 
                     then Just r
                     else Nothing

-- | strict Aeson decoding
decodeStrict :: A.FromJSON a => B.ByteString -> Maybe a
decodeStrict bs = 
    case parseOnly P.json' bs of
      Left _  -> Nothing
      Right v -> case A.fromJSON v of
                   A.Success a -> Just a
                   _           -> Nothing

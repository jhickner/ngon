{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Control.Applicative
import qualified Data.Aeson as A

import Types


sample = "o/12/foo/314/bar"

json = "{\"e\":\"o/12/bar\", \"p\":12}"


main = print $ parseOnly endpointComponents sample


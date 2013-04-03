{-# LANGUAGE OverloadedStrings #-}

-- address messages to specific clients in order to change session, etc
-- better names for objects so you can getOrCreate "ball", but they're
-- namespaced by instance

-- can't do uploads and stuff from elm (fay?)
-- check out bacon.js
-- https://github.com/raimohanska/bacon.js/wiki/Documentation
-- use elm example to make a bacon websocket

module Snap where

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

--import Control.Monad
import Control.Monad.IO.Class
--import Control.Concurrent
import Control.Applicative

-- socket -> parse json packet -> process it -> possibly send socket response
-- so the pipe proxy includes a client reference so we know if a response is
-- necessary

import Network.WebSockets (WebSockets, Hybi10)
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap


import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.Text as PT
import qualified Data.Aeson as A
import Data.Aeson (ToJSON, FromJSON, decode')
import Data.Text.Encoding (decodeUtf8)



import Types


writeJSON :: (A.ToJSON a, MonadSnap m) => a -> m ()
writeJSON a = do 
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . A.encode $ a

config = 
    setPort 8000 .
    setVerbose False .
    setErrorLog  (ConfigIoLog B.putStrLn) .
    setAccessLog ConfigNoLog $
    defaultConfig 

runWebServer :: IO ()
runWebServer = httpServe config (route routes) where
    routes = [ 
              -- ("/ws", setTimeout 31536000 >> runWebSocketsSnap app)
             ("/api", api)
             , ("/", serveDirectory "static")
             ]



api = do
    -- read the rqPathInfo
    -- if null path, parse body as raw json packets
    -- if path, use it as endpoint
    -- read the requestbody, parse as JSON and use it as payload
    path' <- decodeUtf8 . rqPathInfo <$> getRequest -- ByteString
    mpl <- decode' <$> readRequestBody 4096
    liftIO $ print $ Packet Nothing (parseEndpoint path') mpl
  where
    parseEndpoint t = case PT.parseOnly endpointComponents t of
                        Left _   -> []
                        Right es -> es

-- parsing and routing on path components...


-- HTTP      - lazy bytestring from readRequestBody
-- websocket - bytestring/text
-- socket    - bytestring

-- {"p":"o/foo", "p":{"x":0, "y": 0}}


-- web request happens in snap monad
-- websocket happens in websocket monad
-- run pipe on each websocket message?


main :: IO ()
main = runWebServer


{-
decodePackets :: (Monad m, Proxy p) => () -> Pipe p ByteString Packet m r
decodePackets () = runIdentityP $ forever $ do
    b <- request ()
    case AP.parse A.json' b of
      AP.Done _ v -> case A.fromJSON v of
                       A.Success a -> respond a
                       _           -> return ()
      _           -> return ()

app :: WS.Request -> WebSockets Hybi10 ()
app rq = do
    WS.acceptRequest rq
    go
  where
    go = do
        n <- WS.receiveData :: WebSockets Hybi10 ByteString
        liftIO $ print n
        go
-}

-- TODO: how do I get the sink out?
{-
readWebSocketS :: Proxy p => () -> Consumer p ByteString Snap ()
readWebSocketS () = runIdentityP $
    lift $ runWebSocketsSnap $ \rq -> WS.acceptRequest rq >> go
      where 
        go = do
            n <- WS.receiveData :: WebSockets Hybi10 ByteString
            embed $ respond n
            go
-}
            

    


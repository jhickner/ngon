{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Network.Socket (Socket)
import Network.Socket.ByteString hiding (send)
import TCP
import Types

import Data.Attoparsec
import qualified Data.Aeson as A

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

main = connect "localhost" "8001" $ \(sock,_) -> do
    send sock $ Packet (Just 1) ["u", "socket"] "connect" Nothing Nothing
    send sock $ Packet (Just 1) ["o"] "sub" Nothing Nothing
    socketDecoder sock $ \p -> print (p :: Packet)
  where
    send sock = sendAll sock . toStrict . A.encode
  
toStrict = B.concat . BL.toChunks

-- | looped socket JSON decoder
socketDecoder :: A.FromJSON a => Socket-> (a -> IO ()) -> IO ()
socketDecoder sock f = loop Nothing Nothing
  where 
    loop mpartial mbytes = do
        bytes <- maybe (recv sock 4096) return mbytes
        unless (B.null bytes) $ -- null recv indicates closed connection
          case maybe (parse A.json') feed mpartial bytes of
            Fail{}        -> loop Nothing Nothing
            k@Partial{}   -> loop (Just k) Nothing
            Done bytes' c -> do
                case A.fromJSON c of
                  A.Success a -> f a
                  _           -> return ()
                loop Nothing $ if B.null bytes' 
                                 then Nothing 
                                 else Just bytes'

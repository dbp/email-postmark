{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.Postmark
  (postmark)
  where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec as A
{-import Control.Monad-}
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString as BS
{-import qualified Data.ByteString.Char8 as B8-}
import Data.Maybe (fromJust)
import Network.HTTP hiding (getRequest)
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.URI (parseURI)
{-import Control.Monad.Trans-}


maybeAesonResult (Error _) = Nothing
maybeAesonResult (Success a) = Just a

strictifyBS = BS.concat . LBS.toChunks

postmark token from subject tag body to = do
        resp <- simpleHTTP (Request 
                              (fromJust $ parseURI "http://api.postmarkapp.com/email")
                              POST
                              [ mkHeader HdrAccept "application/json"
                              , mkHeader HdrContentType "application/json"
                              , mkHeader (HdrCustom "X-Postmark-Server-Token") token
                              , mkHeader HdrContentLength (show $ LBS.length rqbdy)]
                              rqbdy) >>= getResponseBody
        -- there has to be a cleaner way to do this...
        let val = (parseMaybe (.: "Message")) =<< (maybeAesonResult.fromJSON) =<< (A.maybeResult $ A.parse json $ strictifyBS resp)
        return $ maybe False (== ("OK"::BS.ByteString)) val
  where rqbdy = encode $ M.fromList 
                        [ ("From" :: BS.ByteString, from)
                        , ("To", to)
                        , ("Subject", subject)
                        , ("Tag", tag)
                        , ("TextBody", body)
                        ]
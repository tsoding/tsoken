{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Printf
import URI.ByteString

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

secretToken :: B.ByteString
secretToken = "just-a-test-token"

authApp :: Application
authApp req respond = do
  let token = do
        uri <- lookup "X-Original-URI" $ requestHeaders req
        RelativeRef {rrQuery = query} <-
          rightToMaybe $ parseRelativeRef strictURIParserOptions uri
        lookup "token" $ queryPairs query
  if token == Just secretToken
    then respond $ responseLBS status200 [("Set-Cookie", "token=" <> secretToken)] "Okayeg"
    else respond $ responseLBS status403 [] "DansGame"

main :: IO ()
main = do
  let port = 8082
  printf "Listening on http://localhost:%d/\n" port
  run port authApp

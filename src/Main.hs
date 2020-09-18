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
import Control.Applicative
import Cookie

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

type Token = B.ByteString

secretToken :: Token
secretToken = "just-a-test-token"

tokenFromURI :: Request -> Maybe Token
tokenFromURI request = do
  let uri = rawQueryString request
  RelativeRef {rrQuery = query} <- rightToMaybe $ parseRelativeRef strictURIParserOptions uri
  lookup "token" $ queryPairs query

tokenFromXOriginalURI :: Request -> Maybe Token
tokenFromXOriginalURI request = do
  xOriginaURI <- lookup "X-Original-URI" $ requestHeaders request
  RelativeRef {rrQuery = query} <- rightToMaybe $ parseRelativeRef strictURIParserOptions xOriginaURI
  lookup "token" $ queryPairs query

tokenFromCookie :: Request -> Maybe Token
tokenFromCookie request = do
  cookieString <- lookup "Cookie" $ requestHeaders request
  cookies <- parseCookies cookieString
  lookup "token" cookies

authApp :: Application
authApp req respond = do
  let token =
        tokenFromCookie req <|> tokenFromXOriginalURI req <|> tokenFromURI req
  if token == Just secretToken
    then respond $
         responseLBS
           status200
           [("Set-Cookie", "token=" <> secretToken)]
           "Okayeg"
    else respond $ responseLBS status403 [] "DansGame"

main :: IO ()
main = do
  let port = 8082
  printf "Listening on http://localhost:%d/\n" port
  run port authApp

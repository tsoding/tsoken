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
import System.Environment

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

type Token = B.ByteString

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

authApp :: Token ->  Application
authApp secretToken req respond = do
  let token =
        tokenFromURI req <|> tokenFromXOriginalURI req <|> tokenFromCookie req
  if token == Just secretToken
    then respond $
         responseLBS
           status200
           [("Set-Cookie", "token=" <> secretToken)]
           "Okayeg"
    else respond $ responseLBS status403 [] "DansGame"

mainWithArgs :: [String] -> IO ()
mainWithArgs (tokenFilePath:_) = do
  secretToken <- B.strip <$> B.readFile tokenFilePath
  let port = 8082
  printf "Listening on http://localhost:%d/\n" port
  run port (authApp secretToken)
mainWithArgs _ = error "Usage: tsoken <token-file-path>"

main :: IO ()
main = getArgs >>= mainWithArgs

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Text.Printf
import Data.List
import Data.Maybe
import Network.URI
import qualified Data.ByteString.Char8 as B

listAsPair :: [a] -> Maybe (a, a)
listAsPair [x, y] = Just (x, y)
listAsPair _ = Nothing

extractTokenFromHeaders :: [Header] -> Maybe String
extractTokenFromHeaders headers = do
  (_, originalURI) <- find (((==) "X-Original-URI") . fst) headers
  uri <- parseRelativeReference $ B.unpack originalURI
  pair <- B.uncons $ B.pack $ uriQuery uri
  case pair of
    ('?', args) -> do
      arg <-
        find ((== "token") . fst) $
        mapMaybe (listAsPair . B.split '=') $ B.split '&' args
      return $ B.unpack $ snd arg
    _ -> Nothing

secretToken :: String
secretToken = "just-a-test-token"

authApp :: Application
authApp req respond = do
  let headers = requestHeaders req
  let token = extractTokenFromHeaders headers
  if token == Just secretToken
  then respond $ responseLBS status200 [] "Okayeg"
  else respond $ responseLBS status403 [] "DansGame"

main :: IO ()
main = do
  let port = 8082
  printf "Listening on http://localhost:%d/\n" port
  run port authApp

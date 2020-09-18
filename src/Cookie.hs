module Cookie where

import qualified Data.ByteString.Char8 as B

type Cookie = (B.ByteString, B.ByteString)
type Cookies = [Cookie]

parseCookie :: B.ByteString -> Maybe Cookie
parseCookie input = do
  let (name, rest) = B.span (/= '=') $ B.strip input
  value <-
    case B.uncons rest of
      Just ('=', rest) ->
        case B.uncons rest of
          Just ('"', rest) ->
            case B.span (/= '"') rest of
              (value, "\"") -> return value
              _ -> Nothing
          Just (_, _) -> return rest
          _ -> Nothing
      _ -> Nothing
  return (name, value)

parseCookies :: B.ByteString -> Maybe Cookies
parseCookies = traverse parseCookie . B.split ';'

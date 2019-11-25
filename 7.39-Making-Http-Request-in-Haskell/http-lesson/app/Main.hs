module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status

-- 39.3
myToken :: BC.ByteString
myToken = ""

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

-- 39.5
buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path = setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

-- 39.7
main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status /= 200
    then do
      let status = getResponseStatus response
      print $ statusMessage status
  else do
    print "saving reques to a file"
    let jsonBody = getResponseBody response
    L.writeFile "data.json" jsonBody

-- Q39.1
buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNOSSL token host method path = setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestPort 443
  $ defaultRequest

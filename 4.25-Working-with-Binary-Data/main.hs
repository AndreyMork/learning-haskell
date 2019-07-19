{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

-- 25.1
sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- 25.2
-- sampleString :: String
-- sampleString = B.unpack sampleBytes -- error

-- QC25.1
bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack


-- 25.13
nagarjunaBC :: BC.ByteString
nagarjunaBC = "नागर्जुनॅ"

-- 25.14
nagarjunaText :: T.Text
nagarjunaText = "नागर्जुनॅ"

-- 25.15
nagarjunaByteString :: B.ByteString
nagarjunaByteString = (BC.pack . T.unpack) nagarjunaText

-- 25.16
nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText

-- Q25.1
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  text <- TIO.readFile fileName
  let numberOfChars = T.length text
  let numberOfBytes = (B.length . E.encodeUtf8) text
  let message = mconcat ["Number of chars: ", show numberOfChars, "\n", "Number of bytes: ", show numberOfBytes]
  putStrLn message

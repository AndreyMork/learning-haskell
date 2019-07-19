{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO

-- 23.1
firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- 23.4
aWord :: T.Text
aWord = "Cheese"

-- 23.5
sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- 23.7
breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

-- Q23.2
toInts :: LT.Text -> [Int]
toInts = map read . map LT.unpack . LT.lines


main :: IO ()
main = do
  userInput <- LTIO.getContents
  let numbers = toInts userInput
  print (sum numbers)

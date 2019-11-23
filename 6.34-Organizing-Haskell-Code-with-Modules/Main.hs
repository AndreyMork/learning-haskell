-- -- 34.5
-- module Main where

-- -- 34.3
-- head :: Monoid a => [a] -> a
-- head (x:xs) = x
-- head [] = mempty

-- -- 34.4
-- example :: [[Int]]
-- example = []

-- QC 34.1
-- length :: Int
-- length = 8
-- Main.length

-- 34.6
module Main where
import qualified Palindrome

main :: IO()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  let response = if Palindrome.isPalindrome text
                 then "it is!"
                 else "it's not"
  print response

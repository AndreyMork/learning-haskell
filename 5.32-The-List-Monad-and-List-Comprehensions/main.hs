import Control.Monad
import Data.Char

-- 32.2
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powerOfTwo = 2 ^ value
  let powerOfThree = 3 ^ value
  return (powerOfTwo, powerOfThree)

-- QC 32.1
valueAndSquarePairs :: [(Int, Int)]
valueAndSquarePairs = do
  value <- [1 .. 10]
  return (value, value ^ 2)

-- QC 32.2
guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter predicate list = do
  value <- list
  guard(predicate value)
  return value

-- 32.3
evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n ^ 2
  guard(even nSquared)
  return nSquared

-- QC 32.3
capitalize :: String -> String
capitalize (first:rest) = (toUpper first):rest

answer :: [String]
answer = [
  "Mr. " ++ capitalized
  | color <- ["brown", "blue", "pink", "orange"]
  , let capitalized = capitalize color]

-- Q32.1
monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [date | end <- ends, date <- [1 .. end]]

-- Q32.2
datesDo :: [Int] -> [Int]
datesDo ends = do
  end <- ends
  date <- [1 .. end]
  return date

datesMonad :: [Int] -> [Int]
datesMonad ends =
  ends >>=
  (\end -> [1 .. end] >>=
  (\date -> return date))

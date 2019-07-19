import Data.List.Split


-- 22.7
-- main :: IO ()
-- main = do
--   userInput <- getContents
--   mapM_ print userInput

-- QC22.3
-- main :: IO ()
-- main = do
--   userInput <- getContents
--   let reversedInput = reverse userInput
--   putStrLn reversed

-- 22.8
sempleData = ['6', '2', '\n', '2', '1', '\n']

-- 22.9
myLines = splitOn "\n"

-- 22.10
toInts :: String -> [Int]
toInts = map read . lines

-- 22.11
-- main :: IO ()
-- main = do
--   userInput <- getContents
--   let numbers = toInts userInput
--   print (sum numbers)

-- QC22.4
sumOfSquares :: Num a => [a] -> a
sumOfSquares = sum . map (^ 2)

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sumOfSquares numbers)

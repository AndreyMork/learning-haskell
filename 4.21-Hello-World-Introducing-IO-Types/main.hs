import System.Random
import qualified Data.Map as Map


-- 21.1
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

-- main :: IO ()
-- main = do
--   putStrLn "Hello! What's your name?"
--   name <- getLine
--   let statement = helloPerson name
--   putStrLn statement

-- 21.2

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

-- main :: IO ()
-- main = do
--   dieRoll <- randomRIO (minDie, maxDie)
--   putStrLn (show dieRoll)

-- Q21.1
sampleMap :: Map.Map Int String
sampleMap = Map.fromList [(1, "Will")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 1 sampleMap
  let statement = helloPerson name
  return statement

-- Q21.2
fib :: Integer -> Integer
fib 0 = 0
fib n = calculateFib 1 (1, 0)
  where
    calculateFib ind (curr, prev)
      | (ind == n) = curr
      | otherwise = calculateFib (ind + 1) (curr + prev, curr)

main :: IO ()
main = do
  putStrLn "Input n:"
  n <- getLine
  let fibN = fib (read n)
  putStrLn (show fibN)

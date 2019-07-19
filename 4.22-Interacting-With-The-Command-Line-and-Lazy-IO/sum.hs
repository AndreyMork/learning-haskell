-- 22.1
import System.Environment
import Control.Monad


-- main :: IO ()
-- main = do
--   args <- getArgs
-- 22.3
--   mapM_ putStrLn args

-- QC 22.1
-- main :: IO ()
-- main = do
--   values <- mapM (\_ -> getLine) [1 .. 3]
--   mapM_ putStrLn values

-- 22.4
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let linesToRead = if length args > 0
--       then read (head args)
--       else 0 :: Int
--   print linesToRead

-- 22.5
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let linesToRead = if length args > 0
--       then read (head args)
--       else 0 :: Int
--   numbers <- replicateM linesToRead getLine
--   print "sum goes here"

-- 22.6
main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
      then read (head args)
      else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)

-- QC22.2
myReplicateM :: Monad m => Int -> m a -> m a
myReplicateM action n = mapM (\_ -> action) [1 .. n]

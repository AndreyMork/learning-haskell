import System.Environment
import System.IO

-- 24.6
getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = length input
    wordCount = (length . words) input
    lineCount = (length . lines) input

-- 24.7
countsText :: (Int, Int, Int) -> String
countsText (charCount, wordCount, lineCount) = unwords [
  "chars:", show charCount,
  " words:", show wordCount,
  " lines:", show lineCount]

-- 24.8
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   input <- readFile fileName
--   let summary = (countsText . getCounts) input
--   appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
--   putStrLn summary
--

-- 24.9
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   file <- openFile fileName ReadMode
--   input <- hGetContents file
--   hClose file
--   let summary = (countsText . getCounts) input
--   appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
--   putStrLn summary

-- 24.10
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- 24.2
-- main :: IO ()
-- main = do
--   myFile <- openFile "hello.txt" ReadMode
--   hClose myFile
--   putStrLn "done!"

-- 24.3
-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   firstLine <- hGetLine helloFile
--   putStrLn firstLine
--   secondLine <- hGetLine helloFile
--   goodbyeFile <- openFile "goodbye.txt" WriteMode
--   hPutStrLn goodbyeFile secondLine
--   hClose helloFile
--   hClose goodbyeFile
--   putStrLn "done!"

-- 24.4
-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   hasLine <- hIsEOF helloFile
--   firstLine <- if not hasLine
--     then hGetLine helloFile
--     else return "empty"
--   putStrLn "done!"


-- Q24.1
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let (source:dest:_) = args
--   fileContent <- TIO.readFile source
--   TIO.writeFile dest fileContent


-- Q24.2
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  fileContent <- TIO.readFile fileName
  TIO.writeFile fileName (T.toUpper fileContent)

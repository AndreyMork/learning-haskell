import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random
import Control.Monad

-- 25.3
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   imageFile <- BC.readFile fileName
--   glitched <- return imageFile
--   let glitchedFileName = mconcat ["glitched_", fileName]
--   BC.writeFile glitchedFileName glitched
--   print "all done"

-- 25.4
intToChar :: Int -> Char
intToChar int = toEnum (mod int 255)

-- 25.5
intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- 25.6
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.tail rest
    newChar = intToBC charVal

-- 25.7
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   imageFile <- BC.readFile fileName
--   glitched <- randomReplaceByte imageFile
--   let glitchedFileName = mconcat ["glitched_", fileName]
--   BC.writeFile glitchedFileName glitched
--   print "all done"

-- QC 25.3
randomChar :: IO Char
randomChar = randomRIO (minBound :: Char, maxBound :: Char)

-- 25.8
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

-- 25.9
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

-- 25.10
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   imageFile <- BC.readFile fileName
--   glitched <- randomSortSection imageFile
--   let glitchedFileName = mconcat ["glitched_", fileName]
--   BC.writeFile glitchedFileName glitched
--   print "all done"

-- 25.12
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   imageFile <- BC.readFile fileName
--   glitched <- foldM (\bytes f -> f bytes) imageFile [
--     randomReplaceByte,
--     randomSortSection,
--     randomReplaceByte,
--     randomSortSection,
--     randomReplaceByte]
--   let glitchedFileName = mconcat ["glitched_", fileName]
--   BC.writeFile glitchedFileName glitched
--   print "all done"

-- QC 25.4
-- glitchActions :: [BC.ByteString -> IO BC.ByteString]
-- glitchActions = [
--   randomReplaceByte,
--   randomSortSection,
--   randomReplaceByte,
--   randomSortSection,
--   randomReplaceByte]

-- Q25.2
reverseBytes ::  Int -> Int -> BC.ByteString -> BC.ByteString
reverseBytes start size bytes = mconcat [beforeSection, reversedSection, afterSection]
  where
    (beforeSection, rest) = BC.splitAt start bytes
    (section, afterSection) = BC.splitAt size rest
    reversedSection = BC.reverse section

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
  let bytesLength = BC.length bytes
  start <- randomRIO (1, bytesLength)
  size <- randomRIO (1, bytesLength - start)
  return (reverseBytes start size bytes)


randomChoose :: [a] -> IO a
randomChoose [] = error "list is empty"
randomChoose list = do
  let listLength = length list
  randomInd <- randomRIO (0, listLength - 1)
  return (list !! randomInd)


type GlitchAction = BC.ByteString -> IO BC.ByteString
randomGlitchTransformation :: [GlitchAction] -> BC.ByteString -> IO BC.ByteString
randomGlitchTransformation glitchActions bytes = do
  action <- randomChoose glitchActions
  glitchedBytes <- action bytes
  continue <- randomRIO (0 :: Int, 10 :: Int)
  if (continue < 7)
    then (randomGlitchTransformation glitchActions glitchedBytes)
  else (return glitchedBytes)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  let glitchActions = [randomReplaceByte, randomSortSection, randomReverseBytes]
  glitched <- randomGlitchTransformation glitchActions imageFile
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

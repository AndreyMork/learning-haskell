import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char (isPunctuation, isSpace)
import Data.Text as T

-- assert :: Bool -> String -> String -> IO ()
-- assert test passStatement failStatement = if test
--                                           then putStrLn passStatement
--                                           else putStrLn failStatement
--
-- main :: IO ()
-- main = do
--   putStrLn "Runnig tests..."
--   assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
--   assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
--   assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"
--   assert ((isPalindrome) "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
--   assert ((isPalindrome) ":racecar:") "passed ':racecar:'" "FAIL: ':racecar:'"
--   putStrLn "done!"

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

prop_spaceInvariant text = isPalindrome text == isPalindrome noSpaceText
  where noSpaceText = T.filter (not . isSpace) text

prop_lowerInvariant text = isPalindrome text == isPalindrome loweredText
  where loweredText = T.toLower text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  quickCheck prop_reverseInvariant
  quickCheck prop_spaceInvariant
  quickCheck prop_lowerInvariant
  putStrLn "done!"

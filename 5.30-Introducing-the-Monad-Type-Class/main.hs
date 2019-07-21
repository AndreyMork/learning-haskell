import qualified Data.Map as Map

-- 30.2
type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [
  (1, "nYarlathoTep"),
  (2, "KINGinYELLOW"),
  (3, "dagon1997"),
  (4, "rcarter1919"),
  (5, "xCTHULHUx"),
  (6, "yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [
  ("nYarlathoTep", 2000),
  ("KINGinYELLOW", 15000),
  ("dagon1997", 300),
  ("rcarter1919", 12),
  ("xCTHULHUx", 50000),
  ("yogSOThoth", 150000)]

-- 30.3
-- creditsFromId :: GamerId -> Maybe PlayerCredits

-- 30.4
lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

-- 30.5
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

-- 30.6
-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = altLookupCredits (lookupUserName id)

-- 30.7
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

-- 30.8
type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [
  (1001, 1),
  (1002, 2),
  (1003, 3),
  (1004, 4),
  (1005, 5),
  (1006, 6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

-- 30.9
creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

-- 30.10
echo :: IO ()
echo = getLine >>= putStrLn
--
-- main :: IO ()
-- main = echo

-- QC30.3
-- readInt :: IO
-- readInt = read <$> getLine
--
-- printDouble :: Int -> IO ()
-- printDouble n = print (n * 2)
--
-- readInt >>= printDouble

-- 30.11
echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String and we'll echo it!" >>
  getLine >>= putStrLn

-- 30.12
askForName :: IO ()
askForName = putStrLn "What is your name?"

-- 30.13
nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

-- main :: IO ()
-- main = do
--   askForName
--   name <- getLine
--   let statement = nameStatement name
--   putStrLn statement

main :: IO ()
main = askForName >>
  getLine >>=
  (return . nameStatement) >>=
  putStrLn

-- Q30.1
allFMapM :: Monad m => (a -> b) -> m a -> m b
allFMapM f x = x >>= (return . f)

-- Q30.2
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp f x = f >>= (\y -> x >>= (return . y))

-- >>= :: m a -> (a -> m b) -> m b
-- Q30.3
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just value) f = f value

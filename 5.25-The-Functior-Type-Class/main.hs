import qualified Data.Map as Map


-- 27.1
successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

-- 27.2
incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

-- QC27.1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just string) = Just (reverse string)

-- 27.3
-- instance Functor Maybe where
--   fmap func (Just n) = Just (func n)
--   fmap func Nothing = Nothing

-- 27.4
successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

-- 27.5
data RobotPart = RobotPart {
  name :: String,
  description :: String,
  cost :: Double,
  count :: Int
} deriving Show

-- 27.6
leftArm :: RobotPart
leftArm = RobotPart {
  name = "left arm",
  description = "left arm for face punching!",
  cost = 1000.0,
  count = 3
}

rightArm :: RobotPart
rightArm = RobotPart {
  name = "right arm",
  description = "right arm for kind hand gestures",
  cost = 1025.00,
  count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
  name = "robot head",
  description = "this head looks mad",
  cost = 5092.25,
  count = 2
}

-- 27.7
type Html = String

wrapInTag :: String -> String -> Html
wrapInTag tag str = openingTag <> str <> closingTag
  where
    openingTag = "<" <> tag <> ">"
    closingTag = "</" <> tag <> ">"

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [
  wrapInTag "h2" partName,
  wrapInTag "p" (wrapInTag "h3" "desc" <> partDesc),
  wrapInTag "p" (wrapInTag "h3" "cost" <> partCost),
  wrapInTag "p" (wrapInTag "h3" "count" <> partCount)]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

-- 27.8
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

-- 27.9
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- 27.10
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

--27.11
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

-- 27.12
-- allPartsHtml :: [Html]
-- allPartsHtml = renderHtml <$> allParts

-- 27.12
allPartsHtml :: [Html]
allPartsHtml = map renderHtml allParts

-- 27.14
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- 27.15
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Q27.1
data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box x) = Box (f x)

morePresents :: Int -> Box a -> Box [a]
morePresents n (Box x) = Box (presents)
  where presents = take n (repeat x)

-- Q27.2
myBox :: Box Int
myBox = Box 1

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box a) = a

-- Q27.3
printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "item not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
  putStrLn "enter a part number"
  partNo <- getLine
  let part = Map.lookup (read partNo) partsDB
  printCost (cost <$> part)

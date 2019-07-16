import qualified Data.Map as Map
import Data.List


data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- 19.1
-- data Maybe a = Nothing | Just a

-- 19.2
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

-- 19.3
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where
    getContents id = Map.lookup id catalog

-- 19.4
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- 19.5
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (== Just organ) available)

-- 19.6
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

-- 19.7
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

-- 19.8
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

-- 19.9
organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

-- QC 19.2
numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just n) = n

-- 19.10
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

-- 19.11
process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

-- 19.12
-- processRequest :: Int -> Map.Map Int Organ -> String
-- processRequest id catalog = report (process organ)
--   where organ = Map.lookup id catalog

-- 19.13
processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"

-- 19.14
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

-- Q19.1
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter isNothing

-- Q19.2
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just value) = Just (f value)

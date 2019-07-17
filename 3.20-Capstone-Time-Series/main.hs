-- 20.1
import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

-- 20.2
file1 :: [(Int,Double)]
file1 = [
  (1, 200.1),
  (2, 199.5),
  (3, 199.4),
  (4, 198.9),
  (5, 199.0),
  (6, 200.2),
  (9, 200.3),
  (10, 201.2),
  (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [
  (11, 201.6),
  (12, 201.5),
  (13, 201.5),
  (14, 203.5),
  (15, 204.9),
  (16, 207.1),
  (18, 210.5),
  (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [
  (10, 201.2),
  (11, 201.6),
  (12, 201.5),
  (13, 201.5),
  (14, 203.5),
  (17, 210.5),
  (24, 215.1),
  (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [
  (26, 219.8),
  (27, 220.5),
  (28, 223.8),
  (29, 222.8),
  (30, 223.8),
  (31, 221.7),
  (32, 222.3),
  (33, 220.8),
  (34, 219.4),
  (35, 220.1),
  (36, 220.6)]

-- 20.3
data TimeSeries a = TimeSeries [Int] [Maybe a]

-- 20.4
createTimeSeries :: [Int] -> [a] -> TimeSeries a
createTimeSeries times values = TimeSeries completeTimes extendedValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    extendedValues = map (\value -> Map.lookup value timeValueMap) completeTimes

-- 20.5
fileToTimeSeries :: [(Int, a)] -> TimeSeries a
fileToTimeSeries timeValuePairs = createTimeSeries times values
  where (times, values) = unzip timeValuePairs

-- 20.6
showTimeValuePair :: Show a => Int -> (Maybe a) -> String
showTimeValuePair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTimeValuePair time Nothing = mconcat [show time, "|NA\n"]

-- 20.7
instance Show a => Show (TimeSeries a) where
  show (TimeSeries times values) = mconcat rows
    where rows = zipWith showTimeValuePair times values

-- 20.8
timeSeries1 :: TimeSeries Double
timeSeries1 = fileToTimeSeries file1

timeSeries2 :: TimeSeries Double
timeSeries2 = fileToTimeSeries file2

timeSeries3 :: TimeSeries Double
timeSeries3 = fileToTimeSeries file3

timeSeries4 :: TimeSeries Double
timeSeries4 = fileToTimeSeries file4

-- 20.9
insertMaybePair :: Ord key => Map.Map key value -> (key, Maybe value) -> Map.Map key value
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

-- 20.10
combineTimeSeries :: TimeSeries a -> TimeSeries a -> TimeSeries a
combineTimeSeries (TimeSeries [] []) timeSeries2 = timeSeries2
combineTimeSeries timeSeries1 (TimeSeries [] []) = timeSeries1
combineTimeSeries (TimeSeries times1 values1) (TimeSeries times2 values2) = TimeSeries completeTimes combinedValues
  where
    bothTimes = mconcat [times1, times2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    timeValueMap = foldl insertMaybePair Map.empty (zip times1 values1)
    updatedMap = foldl insertMaybePair timeValueMap (zip times2 values2)
    combinedValues = map (\value -> Map.lookup value updatedMap) completeTimes

-- 20.11
instance Semigroup (TimeSeries a) where
  (<>) = combineTimeSeries

-- 20.12
instance Monoid (TimeSeries a) where
  mempty = TimeSeries [] []
  mappend = (<>)

-- 20.13
timeSeriesAll :: TimeSeries Double
timeSeriesAll = mconcat [timeSeries1, timeSeries2, timeSeries3, timeSeries4]

-- 20.14
mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

meanTimeSeries :: (Real a) => TimeSeries a -> Maybe Double
meanTimeSeries (TimeSeries _ []) = Nothing
meanTimeSeries (TimeSeries times values)
  | (all isNothing values) = Nothing
  | otherwise = Just average
  where
    average = mean cleanValues
    cleanValues = map fromJust justValues
    justValues = filter isJust values

-- 20.15
type CompareFunction a = a -> a -> a
type TimeSeriesCompareFunction a = CompareFunction (Int, Maybe a)

makeTimeSeriesCompare :: Eq a => CompareFunction a -> TimeSeriesCompareFunction a
makeTimeSeriesCompare f = newF
  where
    newF (times1, Nothing) (times2, Nothing) = (times1, Nothing)
    newF (_, Nothing) (times, values) = (times, values)
    newF (times, values) (_, Nothing) = (times, values)
    newF (times1, Just values1) (times2, Just values2) = if (f values1 values2) == values1
      then (times1, Just values1)
    else (times2, Just values2)

-- 20.16
compareTimeSeries :: Eq a => (a -> a -> a) -> TimeSeries a -> Maybe (Int, Maybe a)
compareTimeSeries f (TimeSeries [] []) = Nothing
compareTimeSeries f (TimeSeries times values)
  | all isNothing values = Nothing
  | otherwise = Just best
  where
    pairs = zip times values
    best = foldl (makeTimeSeriesCompare f) (0, Nothing) pairs

-- 20.17
minTimesSeries :: Ord a => TimeSeries a -> Maybe(Int, Maybe a)
minTimesSeries = compareTimeSeries min

maxTimesSeries :: Ord a => TimeSeries a -> Maybe(Int, Maybe a)
maxTimesSeries = compareTimeSeries max

-- 20.18
-- 20.19
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

-- 20.20
diffTimeSeries :: Num a => TimeSeries a -> TimeSeries a
diffTimeSeries (TimeSeries [] []) = TimeSeries [] []
diffTimeSeries (TimeSeries times values) = TimeSeries times (Nothing:diffValues)
  where
    diffValues = zipWith diffPair shiftValues values
    shiftValues = tail values

-- 20.21
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe values
  | (any isNothing values) = Nothing
  | otherwise = Just average
  where
    average = mean (map fromJust values)

-- 20.22
movingAverage :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAverage [] n = []
movingAverage values n
  | (length nextValues == n) = meanMaybe nextValues:(movingAverage restValues n)
  | otherwise = []
  where
    nextValues = take n values
    restValues = tail values

-- 20.23
movingAverageTimeSeries :: (Real a) => TimeSeries a -> Int -> TimeSeries Double
movingAverageTimeSeries (TimeSeries [] []) n = TimeSeries [] []
movingAverageTimeSeries (TimeSeries times values) n = TimeSeries times smoothedValues
  where
    smoothedValues = mconcat [nothings, ma, nothings]
    nothings = replicate (div n 2) Nothing
    ma = movingAverage values n

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

-- 42.1
aLargeList :: [Int]
aLargeList = [1 .. 10000000]

-- 42.2
aLargeArray :: UArray Int Int
aLargeArray = array (0, 9999999) []

-- 42.3
aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList

-- 42.4
zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 9) [(3, True)]

-- 42.5
oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 10) $ zip [1 .. 10] (cycle [True])

-- QC 42.1
qcArray :: UArray Int Bool
qcArray = array (0, 4) [(0, True), (1, True), (2, False)]

-- 42.6
beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) $ zip [0 .. 3] (cycle [0])

-- 42.7
updatedBeansInBuckets :: UArray Int Int
updatedBeansInBuckets = beansInBuckets // [(1, 5), (3, 6)]

-- 42.9
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

-- 42.11
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

-- 42.12
myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

-- 42.13
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort anArray = runSTUArray $ do
  stArray <- thaw anArray
  let end = (snd . bounds) anArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - 1)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray

-- Q42.1
testArray1 :: UArray Int Bool
testArray1 = listArray (0, 4) [True, True, True, True, True]

testArray2 :: UArray Int Bool
testArray2 = listArray (0, 4) [False, False, False, False, False]

crossover :: Int -> (UArray Int Bool, UArray Int Bool) -> UArray Int Bool
crossover cutoffIndex (array1, array2) = runSTUArray $ do
  let end = (snd . bounds) array1
  stArray <- thaw array1

  forM_ [cutoffIndex .. end] $ \ind -> do
    writeArray stArray ind (array2 ! ind)
  return stArray

-- Q42.2
replaceZeroes :: UArray Int Int -> UArray Int Int
replaceZeroes anArray = runSTUArray $ do
  let (start, end) = bounds anArray
  stArray <- thaw anArray

  forM_ [start .. end] $ \ind -> do
    value <- readArray stArray ind
    when (value == 0) $ do
      writeArray stArray ind (-1)
  return stArray

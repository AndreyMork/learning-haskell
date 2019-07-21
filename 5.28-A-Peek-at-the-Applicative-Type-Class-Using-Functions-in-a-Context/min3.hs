-- 28.7
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

-- 28.8
readInt :: IO Int
readInt = read <$> getLine

-- 28.9
minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

-- 28.10
main = do
  putStrLn "Enter three numbers"
  minInt <- minOfInts
  putStrLn (show minInt ++ " is the smallest")

-- QC28.4
maybeInt :: Maybe Int
maybeInt = minOfThree <$> Just 10 <*> Just 3 <*> Just 6

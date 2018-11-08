merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x < y
    then x:merge xs (y:ys)
  else y:merge (x:xs) ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort list =
  if length list == 1
    then list
  else merge sortedFirstHalf sortedSecondHalf
  where
    middelInd = (length list) `div` 2
    sortedFirstHalf = mergeSort (take middelInd list)
    sortedSecondHalf = mergeSort (drop middelInd list)

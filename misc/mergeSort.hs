splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2

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
    (firstHalf, secondHalf) = splitInHalf list
    sortedFirstHalf = mergeSort firstHalf
    sortedSecondHalf = mergeSort secondHalf

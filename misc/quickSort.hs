quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (pivot:xs) =
  quickSort [x | x <- xs, x < pivot]
  ++ [pivot | x <- xs, x == pivot]
  ++ quickSort [x | x <- xs, x > pivot]

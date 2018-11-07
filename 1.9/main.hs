import Data.Char

addAnA [] = []
addAnA (x:xs) = ("a " ++ x):addAnA xs

squareAll [] = []
squareAll (x:xs) = (x^2):squareAll xs

myMap _ [] = []
myMap fn (x:xs) = (fn x):myMap fn xs

myFilter _ [] = []
myFilter test (x:xs) =
  if test x
    then x:myFilter test xs
  else myFilter test xs

remove _ [] = []
remove test (x:xs) =
  if test x
    then remove test xs
  else x:remove test xs

myProduct xs = foldl (*) 1 xs
concatAll xs = foldl (++) "" xs
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs

myFoldl fn init [] = init
myFoldl fn init (x:xs) = myFoldl fn newInit xs
  where newInit = fn init x

myFoldr fn init [] = init
myFoldr fn init (x:xs) = fn x newRight
  where newRight = myFoldr fn init xs


-- Q9.1
myElem value list = (length filteredList) /= 0
  where filteredList = filter (== value) list

-- Q9.2
isPalindrome text = processedText == reverse processedText
  where
    noSpaces = filter (/= ' ') text
    processedText = map toLower noSpaces

-- Q9.3
harmonic n = sum (take n seriesValues)
  where
    seriesValues = map (1 /) [1.0..]

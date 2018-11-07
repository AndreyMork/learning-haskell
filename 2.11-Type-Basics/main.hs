x :: Int
x = 2

double :: Int -> Int
double n = 2 * n

half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Int -> Int
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (2 * n)

z = read "6"
q = z / 2

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)
makeAddressLambda =
  (\number ->
    (\street ->
      (\town -> (number, street, town))))

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
    then f n
  else n

simple :: a -> a -- type variable
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

-- (Int -> 'Char') -> [Int] -> String
-- myMap show [1, 2, 3, 4]

-- Q11.1
-- What is the type signature for filter ? How is it different from map ?
-- (a -> Bool) -> [a] -> [a]
-- It can't change type

-- Q11.2

-- head :: (a, [a]) -> a
-- a \= [a] so head [] = [] would violate type signature

-- Q11.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x: xs) = myFoldl newInit xs
  where newInit = f init x

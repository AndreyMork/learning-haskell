import qualified Data.Map as Map

-- 21.4
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

-- 21.5
type Pizza = (Double, Double)

-- 21.6
costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

-- 21.7
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas pizza1 pizza2
  | pizzaCost1 < pizzaCost2 = pizza1
  | otherwise = pizza2
  where
    pizzaCost1 = costPerInch pizza1
    pizzaCost2 = costPerInch pizza2

-- 21.8
describePizza :: Pizza -> String
describePizza (size, coast) = "The " ++ show size ++ " pizza " ++ "is cheaper at " ++ show costPerSquareInch ++ " per square inch"
  where costPerSquareInch = costPerInch (size, coast)

-- 21.9
main :: IO ()
main = do
  putStrLn "What is the size of pizza 1?"
  size1 <- getLine
  putStrLn "What is cost of pizza 1?"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2?"
  size2 <- getLine
  putStrLn "What is cost of pizza 2?"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

-- 21.10
costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

-- 21.11
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20), (2, 15.0)]

-- 21.12
maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

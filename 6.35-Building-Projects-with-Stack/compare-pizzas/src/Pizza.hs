module Pizza (
  Pizza,
  comparePizzas,
  describePizza
) where

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas pizza1 pizza2
  | pizzaCost1 < pizzaCost2 = pizza1
  | otherwise = pizza2
  where
    pizzaCost1 = costPerInch pizza1
    pizzaCost2 = costPerInch pizza2

describePizza :: Pizza -> String
describePizza (size, coast) = "The " ++ show size ++ " pizza " ++ "is cheaper at " ++ show costPerSquareInch ++ " per square inch"
  where costPerSquareInch = costPerInch (size, coast)

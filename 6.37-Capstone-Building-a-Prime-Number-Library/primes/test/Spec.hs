import Test.QuickCheck
import Data.Maybe
import Primes

prop_validPrimesOnly val
  | val < 2 = isNothing result
  | val >= length primes = isNothing result
  | otherwise = isJust result
  where result = isPrime val

prop_primesArePrime val
  | result == Just True = length divisors == 0
  | otherwise = True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val
  | result == Just False = length divisors > 0
  | otherwise = True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal val
  | result == Nothing = True
  | otherwise = product (fromJust result) == val
  where result = primeFactors val

prop_allFactorsPrime val
  | result == Nothing = True
  | otherwise = all (== Just True) resultsPrime
  where
    result = primeFactors val
    resultsPrime = map isPrime (fromJust result)

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime

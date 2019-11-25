module Primes where

-- 38.8
data PrimeError = TooLarge | InvalidValue

-- 38.9
instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

primes :: [Int]
primes = sieve [2 .. 10000]

primesMaxBound :: Int
primesMaxBound = length primes

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

-- 38.7
-- isPrime :: Int -> Either String Bool
-- isPrime n
--   | n < 2 = Left "Numbers less than 2 are not candidates fro primes"
--   | n >= primesMaxBound = Left "Value exceeds limits of prime checker"
--   | otherwise = Right (n `elem` primes)

-- 38.10
isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n >= primesMaxBound = Left TooLarge
  | otherwise = Right (n `elem` primes)

-- 38.11
displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

-- 38.12
main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  print (displayResult result)

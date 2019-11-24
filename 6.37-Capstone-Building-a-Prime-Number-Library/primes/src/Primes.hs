module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n >= length primes = Nothing
          | otherwise = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes)
  | n`mod` next == 0 = next:unsafePrimeFactors (n `div` next) (next:primes)
  | otherwise = unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n >= length primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<= n) primes

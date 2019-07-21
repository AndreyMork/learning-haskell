-- 29.1
data Blah a b = Blah (a, b)

-- 29.2
data Box a = Box a

-- 29.3
data ResourceConstrained a = NoResources | Okay a

-- 29.4
doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

-- 29.5
boxPrize :: [Int]
boxPrize = [500, 20000]

-- 29.6
-- totalPrize :: [Int]
-- totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- QC29.4
boxMultiplier :: [Int]
boxMultiplier = [20, 50]

totalPrize :: [Int]
totalPrize = pure (*) <*> doorPrize <*> boxPrize

-- 29.7
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

-- 29.8
testNames :: [String]
testNames = [
  "John Smith",
  "Robert'); DROP TABLE Students;--",
  "Christina NULL",
  "Randall Munroe"]

-- 29.9
testIds :: [Int]
testIds = [
  1337,
  0123,
  999999]

-- 29.10
testScores :: [Int]
testScores = [
  0,
  100000,
  -99999]

-- 29.11
data User = User {
  name :: String,
  gamerId :: Int,
  score :: Int
} deriving Show

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

-- Q29.1
allMap :: Applicative f => (a -> b) -> f a -> f b
allMap f x = pure f <*> x

-- Q29.2
example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> pure ((+) 2 4) <*> pure 6

-- Q29.3
startingBeer :: [Int]
startingBeer = [6, 12]

drinkedBeers :: Int
drinkedBeers = 4

remainingBeer :: [Int]
remainingBeer = (\count -> count - drinkedBeers) <$> startingBeer

friends :: [Int]
friends = [2, 3]

totalPeople  :: [Int]
totalPeople = (+ 2) <$> friends

beersPerGuest :: [Int]
beersPerGuest = [3, 4]

totalBeersNeeded :: [Int]
totalBeersNeeded = pure (*) <*> totalPeople <*> beersPerGuest

beersToPurchase :: Int
beersToPurchase = maximum (pure (-) <*> totalBeersNeeded <*> remainingBeer)

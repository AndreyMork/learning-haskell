import qualified Data.Map as Map

-- 31.1
askForName :: IO ()
askForName = putStrLn "WhatIsYourName?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

-- helloName :: IO ()
-- helloName = askForName >>
--   getLine >>=
--   (return . nameStatement) >>=
--   putStrLn

-- maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
-- maxPairM pair = pair >>=
--   (\(a, b) -> return (max a b))

-- 31.2
helloName :: IO ()
helloName = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)

-- 31.5
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

-- 31.6
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

-- 31.7
data Candidate = Candidate {
  candidateId :: Int,
  codeReview :: Grade,
  cultureFit :: Grade,
  education :: Degree
} deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    tests = [passedCoding, passedCultureFit, educationMin]
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS

-- QC31.2
testCandidate :: Candidate
testCandidate = Candidate {
  candidateId = 1,
  codeReview = A,
  cultureFit = A,
  education = PhD
}

-- 39.9
readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

-- 31.10
readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id:"
  candidateId <- readInt
  putStrLn "enter code grade:"
  codeGrade <- readGrade
  putStrLn "enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "enter education:"
  degree <- readDegree
  return (Candidate {
    candidateId = candidateId,
    codeReview = codeGrade,
    cultureFit = cultureGrade,
    education = degree
  })

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
      then "passed"
      else "failed"
  return statement

-- 31.12
candidate1 :: Candidate
candidate1 = Candidate {
  candidateId = 1,
  codeReview = A,
  cultureFit = A,
  education = BA
}

candidate2 :: Candidate
candidate2 = Candidate {
  candidateId = 2,
  codeReview = C,
  cultureFit = A,
  education = PhD
}

candidate3 :: Candidate
candidate3 = Candidate {
  candidateId = 3,
  codeReview = A,
  cultureFit = B,
  education = MS
}

-- 31.13
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [
  (1, candidate1),
  (2, candidate2),
  (3, candidate3)]

-- 31.14
assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe candidateId = do
  candidate <- Map.lookup candidateId candidateDB
  let passed = viable candidate
  let statement = if passed
      then "passed"
      else "failed"
  return statement

-- 31.15
candidates :: [Candidate]
candidates = [
  candidate1,
  candidate2,
  candidate3]

-- 31.16
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidate = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
      then "passed"
      else "failed"
  return statement

-- 31.18
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
      then "passed"
      else "failed"
  return statement

-- Q31.1
-- Placeholders. See lesson 21.
type Pizza = (Double, Double)

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas a _ = a

describePizza :: Pizza -> String
describePizza _ = "pizza"

-- main :: IO ()
-- main =
--   putStrLn "What is the size of pizza 1" >>
--   getLine >>=
--     (\size1 -> putStrLn "What is the cost of pizza 1" >>
--       getLine >>=
--         (\cost1 -> putStrLn "What is the size of pizza 2" >>
--           getLine >>=
--             (\size2 -> putStrLn "What is the cost of pizza 2" >>
--               getLine >>=
--                 (\cost2 -> putStrLn (
--                   describePizza (
--                     comparePizzas
--                       (read size1, read cost1)
--                       (read size2, read cost2)))))))

-- Q31.2
listMain :: [String]
listMain = do
  size1 <- [10, 10]
  cost1 <- [10, 10]
  size2 <- [10, 10]
  cost2 <- [10, 10]
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

monadMain :: Monad m => m Pizza -> m Pizza -> m String
monadMain mPizza1 mPizza2 = do
  pizza1 <- mPizza1
  pizza2 <- mPizza2
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

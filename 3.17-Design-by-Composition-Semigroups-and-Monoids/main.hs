import Data.List
import Data.Semigroup

-- 17.1
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

-- QC 17.1
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

-- 17.3
-- data Color = Red
--   | Yellow
--   | Blue
--   | Green
--   | Purple
--   | Orange
--   | Brown deriving (Show, Eq)

-- -- 17.4
instance Semigroup Color where
  -- (<>) Red Blue = Purple
  -- (<>) Blue Red = Purple
  -- (<>) Yellow Blue = Green
  -- (<>) Blue Yellow = Green
  -- (<>) Yellow Red = Orange
  -- (<>) Red Yellow = Orange
  -- (<>) a b = if a == b then a else Brown

-- 17.5
-- instance Semigroup Color where
--   (<>) Red Blue = Purple
--   (<>) Blue Red = Purple
--   (<>) Yellow Blue = Green
--   (<>) Blue Yellow = Green
--   (<>) Yellow Red = Orange
--   (<>) Red Yellow = Orange
--   (<>) a b
--     | a == b = a
--     | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
--     | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
--     | otherwise = Brown

-- 17.8
-- type Events = [String]
-- type Probs = [Double]

-- 17.9
data PTable = PTable Events Probs

-- 17.10
-- createPTable :: Events -> Probs -> PTable
-- createPTable events probs = PTable events normalizedProbs
--   where
--     totalProbs = sum probs
--     normalizedProbs = map (/ totalProbs) probs

-- 17.11
-- showPair :: String -> Double -> String
-- showPair event prob = mconcat [event, "|", show prob, "\n"]
--
-- 17.12
-- instance Show PTable where
--   show (PTable events probs) = mconcat pairs
--     where pairs = zipWith showPair events probs

-- 17.13
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

-- 17.14
-- combineEvents :: Events -> Events -> Events
-- combineEvents e1 e2 = cartCombine combiner e1 e2
--   where combiner = (\x y -> mconcat [x, "-", y])
--
-- combineProbs :: Probs -> Probs -> Probs
-- combineProbs p1 p2 = cartCombine (*) p1 p2

-- 17.15
-- instance Semigroup PTable where
--   (<>) ptable1 (PTable [] []) = ptable1
--   (<>) (PTable [] []) ptable2 = ptable2
--   (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
--     where
--       newEvents = combineEvents e1 e2
--       newProbs = combineProbs p1 p2

-- 17.16
-- instance Monoid PTable where
--   mempty = PTable [] []
--   mappend = (<>)

-- 17.17
-- coin :: PTable
-- coin = createPTable ["heads", "tails"] [0.5, 0.5]
--
-- spinner :: PTable
-- spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]


-- Q17.1
data Color = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Clear deriving (Show, Eq)

instance Semigroup Color where
  (<>) Clear x = x
  (<>) x Clear = x
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | otherwise = Brown

instance Monoid Color where
  mempty = Clear
  mappend = (<>)


-- Q17.2
data Probs = Probs [Double]

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs probs1) (Probs probs2) = Probs combinedProbs
  where combinedProbs = cartCombine (*) probs1 probs2

instance Semigroup Probs where
  (<>) = combineProbs
instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)


data Events = Events [String]

combineEvents :: Events -> Events -> Events
combineEvents (Events events1) (Events events2) = Events combinedEvents
  where
    combinedEvents = cartCombine combiner events1 events2
    combiner = (\x y -> mconcat [x, "-", y])

instance Semigroup Events where
  (<>) = combineEvents
instance Monoid Events where
  mempty = Events []
  mappend = (<>)

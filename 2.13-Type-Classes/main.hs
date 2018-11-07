addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String

data Icecream = Chocolate | Vanilla | Strawberry
  deriving (Show, Eq, Ord)

-- Q13.1
-- different max and min values
-- Word == unsigned int

-- Q13.2
inc :: Int -> Int
inc x = x + 1

-- succ (maxBound :: Int) -- Exception: tried to take `succ' of maxBound
-- inc (maxBound :: Int) == (minBound :: Int) -- True

-- Q13.3

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound
    then minBound
  else succ n

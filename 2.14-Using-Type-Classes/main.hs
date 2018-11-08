data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
  deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
  show S1 = "I"
  show S2 = "II"
  show S3 = "III"
  show S4 = "IV"
  show S5 = "V"
  show S6 = "VI"


newtype Name = Name (String, String) deriving (Show, Eq)
instance Ord Name where
  compare (Name (first1, last1)) (Name (first2, last2)) =
    compare (last1, first1) (last2, first2)

names :: [Name]
names = [
  Name ("Emil","Cioran"),
  Name ("Eugene","Thacker"),
  Name ("Friedrich","Nietzsche")]

-- Q14.1
data MyType = I | II | III | IV | V deriving (Enum, Show)
instance Eq MyType where
  (==) a b = (fromEnum a) == (fromEnum b)
instance Ord MyType where
  compare a b = compare (fromEnum a) (fromEnum b)

-- Q14.2

data FiveSidedDice = Side1 | Side2 | Side3 | Side4 | Side5
  deriving (Enum, Eq, Show)
instance Die FiveSidedDice where
  roll n = toEnum (n `mod` 5)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

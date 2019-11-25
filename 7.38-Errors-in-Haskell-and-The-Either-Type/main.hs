import Data.Char (isDigit)

-- 38.1
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

-- 38.2
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n - 1) xs

-- 38.3
myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x

-- 38.4
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- 38.5
myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = do
  first <- maybeHead xs
  rest <- myTakeSafer (n - 1) (Just (tail xs))
  return (first:rest)

-- 38.6
eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x


-- Q38.1
addStrInts :: String -> String -> Either String Int
addStrInts a b
  | notIsNumber a = Left "Left value is not an integer"
  | notIsNumber b = Left "Right value is not an integer"
  | otherwise = Right ((read a) + (read b))
  where
    isNumber = all isDigit
    notIsNumber = not . isNumber

-- Q38.2
safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc x
  | x == maxBound = Nothing
  | otherwise = Just (succ x)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

safeLast :: [a] -> Either String a
safeLast [] = Left "Empty list has no last element"
safeLast xs = safeLast' 10000 xs
  where
    safeLast' :: Int -> [a] -> Either String a
    safeLast' 0 _ = Left "List exceeds safe bound"
    safeLast' _ (x:[]) = Right x
    safeLast' n (x:xs) = safeLast' (n - 1) xs

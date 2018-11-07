myGCD a b =
  if remainder == 0
    then b
  else myGCD b remainder
  where remainder = a `mod` b

-- sayAmount n =
--   case n of
--     1 -> "one"
--     2 -> "two"
--     n -> "a bunch"

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

isEmpty [] = True
isEmpty _ = False

myHead (x:xs) = x
myHead [] = error "No head for empy list"

myTail (_:xs) = xs

-- Q7.1
myTail [] = []

-- Q7.2
gcd2 a 0 = a
gcd2 a b = gcd2 b (a `mod` b)

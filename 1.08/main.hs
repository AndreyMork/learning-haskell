myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest
  where rest = myTake (n - 1) xs

myCycle (first:rest) = first:myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
  else 1 + collatz (n * 3 + 1)

-- Q8.1
myReverse [] = []
myReverse (first:rest) = myReverse(rest) ++ [first]

-- Q8.2
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib 0 = 0
fastFib 1 = 1
fastFib 2 = 1
fastFib n =
  iter 1 1 n
  where
    iter n1 n2 3 = n1 + n2
    iter n1 n2 counter = iter n2 (n1 + n2) (counter - 1)

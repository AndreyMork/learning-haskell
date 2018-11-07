teams = ["red", "yellow", "orange", "blue", "purple"]

simple x = x
longList = [1..]

stillLongList = simple longList
backwardsInfinity = reverse longList

paExample1 = (!!) "dog"
paExample2 = ("dog" !!)
paExample3 = (!! 2)

isPalindrome word = word == reverse word

respond phrase =
  if '!' `elem` phrase
    then "wow!"
  else "uh.. okay"

takeLast n aList = reverse (take n (reverse aList))
dropLast n aList = reverse (drop n (reverse aList))

ones n = take n (cycle [1])
assignToGroups n aList = zip groups aList
  where groups = cycle[1..n]

-- Q6.1
repeat value = cycle [value]

-- Q6.2
subseq start end list = drop start (take end list)

-- Q6.3
inFirstHalf val list =
  elem val firstHalf
  where
    midpoint = div (length list) 2
    firstHalf = take midpoint list

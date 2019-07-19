-- Q22.1

calc :: [String] -> Int
calc (lVal:"+":rVal:rest) = read lVal + read rVal
calc (lVal:"*":rVal:rest) = read lVal * read rVal

main :: IO ()
main = do
  userInput <- getContents
  let values = lines userInput
  print (calc values)

-- Q28.3
import qualified Data.Map as Map

data RobotPart = RobotPart {
  name :: String,
  description :: String,
  cost :: Double,
  count :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
  name = "left arm",
  description = "left arm for face punching!",
  cost = 1000.0,
  count = 3
}

rightArm :: RobotPart
rightArm = RobotPart {
  name = "right arm",
  description = "right arm for kind hand gestures",
  cost = 1025.00,
  count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
  name = "robot head",
  description = "this head looks mad",
  cost = 5092.25,
  count = 2
}

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

getCheapestPart :: RobotPart -> RobotPart -> RobotPart
getCheapestPart part1 part2
  | (cost part1) < (cost part2) = part1
  | otherwise = part2

readInt :: IO Int
readInt = read <$> getLine

printRobot :: Maybe RobotPart -> IO ()
printRobot Nothing = print "missing item"
printRobot (Just robot) = print robot

main :: IO ()
main = do
  putStrLn "enter a part 1 id"
  part1Id <- readInt
  let part1 = Map.lookup part1Id partsDB

  putStrLn "enter a part 2 id"
  part2Id <- readInt
  let part2 = Map.lookup part2Id partsDB

  let cheapestPart = getCheapestPart <$> part1 <*> part2
  printRobot cheapestPart

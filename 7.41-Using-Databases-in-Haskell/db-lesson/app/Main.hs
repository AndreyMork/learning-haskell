module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

-- 41.5
data Tool = Tool {
    toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int
}

-- 41.6
data User = User {
    userId :: Int
  , userName :: String
}

-- 41.7
instance Show User where
  show user = mconcat [show $ userId user, ".) ", userName user]

instance Show Tool where
  show tool = mconcat [
      show $ toolId tool
    , ".) "
    , name tool
    , "\n description: "
    , description tool
    , "\n last returned: "
    , show $ lastReturned tool
    , "\n times borrowed: "
    , show $ timesBorrowed tool
    , "\n"]

-- 49.9
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

withToolsDbConn :: (Connection -> IO ()) -> IO ()
withToolsDbConn = withConn "tools.db"

addUser :: String -> IO ()
addUser userName = withToolsDbConn $
  \conn -> do
    execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
    print "user added"

-- 41.10
checkout :: Int -> Int -> IO ()
checkout userId toolId = withToolsDbConn $
  \conn ->
    execute
      conn
      "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)"
      (userId, toolId)

-- 41.12
instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

-- 41.13
printUsers :: IO ()
printUsers = withToolsDbConn $
  \conn -> do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

-- 41.14
printToolQuery :: Query -> IO ()
printToolQuery q = withToolsDbConn $
  \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $
  "SELECT * FROM tools WHERE id NOT IN (SELECT tool_id FROM checkedout);"

printCheckedout :: IO ()
printCheckedout = printToolQuery $
  "SELECT * FROM tools WHERE id IN (SELECT tool_id FROM checkedout);"

-- 41.15
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn
    "SELECT * FROM tools WHERE id = (?)"
    (Only toolId) :: IO [Tool]
  return (firstOrNothing resp)

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

-- 41.16
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool {
  lastReturned = date,
  timesBorrowed = 1 + timesBorrowed tool
}

-- 41.17
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withToolsDbConn $
  \conn -> do
    let q = "UPDATE TOOLS SET lastReturned = ?, timesBorrowed = ? WHERE id = ?;"
    execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
    print "tool updated"

-- 41.18
updateToolTable :: Int -> IO ()
updateToolTable toolId = withToolsDbConn $
  \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn updatedTool

-- 41.19
checkin :: Int -> IO ()
checkin toolId = withToolsDbConn $
  \conn -> do
    execute conn
      "DELETE FROM checkedout WHERE tool_id = (?)"
      (Only toolId)

-- 41.20
checkInAndUpdate :: Int -> IO ()
checkInAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

-- 41.21
promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- pure read <*> getLine
  print "Enter the id of the tool"
  toolId <- pure read <*> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "enter the id of tool"
  toolId <- pure read <*> getLine
  checkInAndUpdate toolId

-- 41.22
performCommand :: String -> IO ()
performCommand command
  | command == "users" = printUsers >> main
  | command == "tools" = printTools >> main
  | command == "adduser" = promptAndAddUser >> main
  | command == "addtool" = promptAndAddTool >> main
  | command == "checkout" = promptAndCheckout >> main
  | command == "checkin" = promptAndCheckin >> main
  | command == "in" = printAvailable >> main
  | command == "out" = printCheckedout >> main
  | command == "quit" = print "bye!"
  | otherwise = print "Sorry command not found" >> main

-- 41.23
main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command

-- Q41.1
addTool :: String -> String -> IO ()
addTool name description = withToolsDbConn $
  \conn -> do
    execute conn
      "INSERT INTO tools (name, description, timesBorrowed) VALUES (?, ?, 0)"
      (name, description)
    print "tool added"

-- Q41.2
promptAndAddTool :: IO ()
promptAndAddTool = do
  print "Enter tool name"
  toolName <- getLine
  print "Enter tool description"
  toolDesc <- getLine
  addTool toolName toolDesc

-- 16.2
-- data AuthorName = AuthorName String String
-- data Book = Author String String Int

-- 16.3
-- data Book = Book {
--   author :: AuthorName,
--   isbn :: String,
--   title :: String,
--   year :: Int,
--   price :: Double
-- }

data AuthorName = AuthorName {
  firstName :: String,
  lastName :: String
}


-- 16.10
data Creator = AuthorCreator Author | ArtistCreator Artist

-- 16.11
data Author = Author Name

-- 16.12
data Artist = Person Name | Band String

-- 16.13
type FirstName = String
type LastName = String
type MiddleName = String
-- data Name = Name FirstName LastName
--   | NameWithMiddle FirstName MiddleName LastName
--   | TwoInitialWithLastName Char Char LastName

-- 16.14
hpLovecraft :: Creator
hpLovecraft = AuthorCreator (
  Author (TwoInitialWithLastName 'H' 'P' "Lovecraft"))

-- 16.15
data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialWithLastName Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char

-- 16.16
data Book = Book {
  author :: Creator,
  isbn :: String,
  bookTitle :: String,
  bookYear :: Int,
  bookPrice :: Double
}

-- 16.17
data VinylRecord = Vinyl {
  artist :: Creator,
  recordTitle :: String,
  recordYear :: Int,
  recordPrice :: Double
}

-- 16.18
-- data StoreItem = BookItem Book | RecordItem VinylRecord

-- 16.19
data CollectibleToy = CollectibleToy {
  name :: String,
  description :: String,
  toyPrice :: Double
}

-- 16.20
-- data StoreItem = BookItem Book
--   | RecordItem VinylRecord
--   | ToyItem CollectibleToy

-- 16.21
-- price :: StoreItem -> Double
-- price (BookItem book) = bookPrice book
-- price (RecordItem record) = recordPrice record
-- price (ToyItem toy) = toyPrice toy

-- QC 16.3
-- Assume that Creator is an instance of Show. Write a madeBy function that has the type StoreItem -> String and does its best to determine who made the StoreItem.
-- madeBy :: StoreItem -> String
-- madeBy (BookItem book) = show (author book)
-- madeBy (RecordItem record) = show (artist record)
-- madeBy _ = "unknown"

-- Q16.1
-- To further complicate the items in your store, you eventually keep an inventory of free pamphlets. Pamphlets have a title, a description, and a contact field for the orga- nization that provides the pamphlet. Create the Pamphlet type and add it to StoreItem. Additionally, modify the price so that it works with Pamphlet.
data Pamphlet = Pamphlet {
  title :: String,
  pamphletDescription :: String,
  contact :: String
}

data StoreItem = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

-- Q16.2
-- Create a Shape type that includes the following shapes: Circle, Square, and Rectangle. Then write a function to compute the perimeter of a Shape as well as its area.

data Circle = Circle {
  radius :: Double
}

data Square = Square {
  sideLength :: Double
}

data Rectangle = Rectangle {
  width :: Double,
  height :: Double
}

data Shape = CircleShape Circle
  | SquareShape Square
  | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape circle) = radius circle * pi * 2
perimeter (SquareShape square) = sideLength square * 4
perimeter (RectangleShape rectangle) = (width rectangle + height rectangle) * 2

area :: Shape -> Double
area (CircleShape circle) = (radius circle ^ 2) * pi
area (SquareShape square) = sideLength square ^ 2
area (RectangleShape rectangle) = width rectangle * height rectangle

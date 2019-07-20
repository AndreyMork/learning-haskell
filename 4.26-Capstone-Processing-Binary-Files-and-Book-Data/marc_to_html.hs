-- 26.1
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

-- 26.2
type Author = T.Text
type Title = T.Text

-- 26.3
data Book = Book {
  author :: Author,
  title :: Title
} deriving Show

-- 26.4
type Html = T.Text

wrapInTag :: T.Text -> T.Text -> Html
wrapInTag tag text = mconcat [openingTag, text, closingTag]
  where
    openingTag = mconcat ["<", tag, ">"]
    closingTag = mconcat ["</", tag, ">"]

-- 26.5
bookToHtml :: Book -> Html
bookToHtml book = wrapInTag "p" (titleInTags <> " " <> authorInTags)
  where
    titleInTags = wrapInTag "strong" (title book)
    authorInTags = wrapInTag "em" (author book)

-- 26.6
book1 :: Book
book1 = Book {
  title = "The Conspiracy Against the Human Race",
   author = "Ligotti, Thomas"
}

book2 :: Book
book2 = Book {
  title = "A Short History of Decay",
  author = "Cioran, Emil"
}

book3 :: Book
book3 = Book {
  title = "The Tears of Eros",
  author = "Bataille, Georges"
}

-- 26.7
booksToHtml :: [Book] -> Html
booksToHtml books = wrapInTag "html" (mconcat [head, charset, body])
  where
    head = wrapInTag "head" (wrapInTag "title" "books")
    charset = "<meta charset='utf-8'/>"
    body = wrapInTag "body" booksHtml
    booksHtml = (mconcat . (map bookToHtml)) books


myBooks :: [Book]
myBooks = [book1, book2, book3]

-- 26.8
-- main :: IO ()
-- main = TIO.writeFile "books.html" (booksToHtml myBooks)


-- 26.9
type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

-- 26.10
leaderLength :: Int
leaderLength = 24

-- 26.11
getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

-- 26.12
rawToInt :: B.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

-- 26.13
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where
    recordLength = getRecordLength marcStream

-- 26.14
allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream
  | marcStream == B.empty = []
  | otherwise = next : allRecords rest
  where (next, rest) = nextAndRest marcStream

-- main :: IO ()
-- main = do
--   marcStream <- B.readFile "sample.mrc"
--   let marcRecords = allRecords marcStream
--   print (length marcRecords)

-- 26.15
type MarcDirectoryRaw = B.ByteString

-- 26.16
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
  where remainder = B.drop 12 leader

-- 26.17
getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

-- 26.18
getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

-- 26.19
type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

-- 26.20
splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory
  | directory == B.empty = []
  | otherwise = nextEntry : splitDirectory restEntries
  where (nextEntry, restEntries) = B.splitAt dirEntryLength directory

-- 26.21
data FieldMetadata = FieldMetadata {
  tag :: T.Text,
  fieldLength :: Int,
  fieldStart :: Int
} deriving Show

-- 26.22
makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata {
  tag = textTag,
  fieldLength = fieldLength,
  fieldStart = fieldStart
} where
    (tag, rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 tag
    (rawLength, rawStart) = B.splitAt 4 rest
    fieldLength = rawToInt rawLength
    fieldStart = rawToInt rawStart

-- 26.23
getFileMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFileMetadata rawEntries = map makeFieldMetadata rawEntries

-- 26.24
type FieldText = T.Text


-- 26.25
getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

-- 26.26
fieldDelimiter :: Char
fieldDelimiter = toEnum 31

-- 26.7
titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

-- 26.28
lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record
  | length results < 1 = Nothing
  | otherwise = Just (head results)
  where
    metadata = (getFileMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

-- 26.29
lookupSubfield :: (Maybe FieldMetadata) -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record
  | results == [] = Nothing
  | otherwise = Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) subfields

-- 26.30
lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
  where entryMetadata = lookupFieldMetadata aTag record

-- 26.31
lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

-- 26.32
marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

-- 26.33
pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map pairToBook justPairs
  where
    pairToBook (title, author) = Book { author = fromJust author, title = fromJust title }
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

-- 26.34
processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed

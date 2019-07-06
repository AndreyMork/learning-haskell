-- 15.1
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded )

-- 15.2
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize char = toEnum rotation
  where
    halfAlphabet = alphabetSize `div` 2
    offset = fromEnum char + halfAlphabet
    rotation = offset `mod` alphabetSize

-- 15.3
-- largestCharNumber :: Int
-- largestCharNumber = fromEnum (maxBound :: Char)

-- 15.4
rotChar :: Char -> Char
rotChar = rotN alphabetSize
  where
    alphabetSize = 1 + largestCharNumber
    largestCharNumber = fromEnum (maxBound :: Char)

-- 15.5
message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

-- 15.6
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where
    rot4l = rotN alphaSize
    alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)

-- 15.7
data ThreeLetterAlphabet
  = Alpha
  | Beta
  | Kappa
  deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where
    rot3l = rotN alphaSize
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

-- 15.8
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n char = toEnum rotation
  where
    rotation = (fromEnum char + offset) `mod` n
    halfN = n `div` 2
    offset =
      if even n
        then halfN
        else 1 + halfN

-- 15.9
threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3ldecoder vals
  where
    rot3ldecoder = rotNdecoder alphaSize
    alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

-- 15.10
rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where
    rotCharDecoder = rotNdecoder alphaSize
    alphaSize = 1 + fromEnum (maxBound :: Char)


-- 15.11
xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 && not value2) || (not value1 && value2)

-- 15.12
xorPair :: (Bool, Bool) -> Bool
xorPair (value1, value2) = xorBool value1 value2

-- 15.13
xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

-- 15.14
type Bits = [Bool]

-- 15.15
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if (remainder == 0)
    then False : intToBits' nextVal
    else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal = n `div` 2

-- 15.16
maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse (intToBits' n)
    missingBits = maxBits - (length reversedBits)
    leadingFalses = take missingBits (cycle [False])

--  15.17
charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

-- 15.18
bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (2 ^) trueLocations)
  where
    size = length bits
    indices = [size - 1, size - 2 .. 0]
    trueLocations = map snd (filter (\(value,_) -> value == True) (zip bits indices))

-- 15.19
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- 15.20
myPad :: String
myPad = "Shhhhhh"

-- 15.21
myPlainText :: String
myPlainText = "Haskell"

-- 15.22
applyOneTimePad' :: String -> String -> [Bits]
applyOneTimePad' pad plainText =
  map (\(padBit, plainTextBit) -> padBit `xor` plainTextBit) pairs
  where
    pairs = zip padBits plainTextBits
    padBits = map charToBits pad
    plainTextBits = map charToBits plainText

-- 15.23
applyOneTimePad :: String -> String -> String
applyOneTimePad pad plainText = map bitsToChar bitList
  where bitList = applyOneTimePad' pad plainText

-- 15.24
encoderDecoder :: String -> String
encoderDecoder = applyOneTimePad myPad

-- 15.25
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

-- 15.26
data Rot = Rot

-- 15.27
instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

-- 15.28
data OneTimePad = OneTimePad String

-- 15.29
instance Cipher OneTimePad where
  encode (OneTimePad pad) text = applyOneTimePad pad text
  decode (OneTimePad pad) text = applyOneTimePad pad text

-- 15.30
myOneTimePad :: OneTimePad
myOneTimePad = OneTimePad (cycle [minBound .. maxBound])

-- 15.31
-- prng == pseudo-random number generator
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

-- 15.32
examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

-- To explore this on your own, use the PRNG to create a StreamCipher type that can be an instance of the Cipher class. Remember: never use your own crypto in the real world! Assume that this should be used for passing notes only.

generateRandomNumbersStream :: Int -> [Int]
generateRandomNumbersStream seed = randomN:(generateRandomNumbersStream randomN)
  where randomN = examplePRNG seed

generateRandomCharsStream :: Int -> String
generateRandomCharsStream seed = map (bitsToChar . intToBits) (generateRandomNumbersStream seed)

generateRandomOneTimePad :: Int -> OneTimePad
generateRandomOneTimePad seed = OneTimePad (generateRandomCharsStream seed)

data StreamCipher = StreamCipher Int

instance Cipher StreamCipher where
  encode (StreamCipher seed) = encode (generateRandomOneTimePad seed)
  decode = encode

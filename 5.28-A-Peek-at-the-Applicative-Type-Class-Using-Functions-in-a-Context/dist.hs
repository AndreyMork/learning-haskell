import qualified Data.Map as Map

-- 28.1
type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [
  ("Arkham", (42.6054, -70.7829)),
  ("Innsmouth", (42.8250, -70.8150)),
  ("Carcosa", (29.9714, -90.7694)),
  ("New York", (40.7776, -73.9691))]

-- 28.2
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rLat, rLong)
  where
    rLat = toRadians lat
    rLong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rLat1, rLong1) = latLongToRads coords1
    (rLat2, rLong2) = latLongToRads coords2
    dLat = rLat2 - rLat1
    dLong = rLong2 - rLong1
    a = (sin (dLat / 2)) ^ 2 + cos rLat1 * cos rLat2 * (sin (dLong / 2)) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 3961.0

-- 28.3
printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

-- 28.5
maybeInc :: Maybe (Integer -> Integer)
maybeInc = (+) <$> Just 1

-- 28.6
main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination cite name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

-- Q28.1
-- haversineIO :: IO LatLong -> IO LatLong -> IO Double
-- haversineIO coords1 coords2 = do
--   justCoords1 <- coords1
--   justCoords2 <- coords2
--   return (haversine justCoords1 justCoords1)

-- Q28.2
haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO coords1 coords2 = haversine <$> coords1 <*> coords2

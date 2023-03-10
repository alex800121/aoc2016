module Day14 (day14) where


import MyLib
import Data.List (transpose)

raceTime :: Int
raceTime = 2503

raceTimeList = [1 .. raceTime]

data Reindeer = Reindeer {name :: String, speed :: Int, restAt :: Int, cycle :: Int} deriving (Show, Eq)

-- Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
inputParser :: String -> Reindeer
inputParser st = let
  name : _ : _ : s : _ : _ : t : _ : _ : _ : _ : _ : _ : rest : _ = words st
  speed = read s
  restAt = read t
  cy = read t + read rest
  in Reindeer name speed restAt cy

distancOfReindeerAt :: Reindeer -> Int -> Int
distancOfReindeerAt (Reindeer _ speed restAt cycle) i = let
  cycles = i `div` cycle
  additionalTime = min restAt $ i `mod` cycle
  in speed * (cycles * restAt + additionalTime)

day14b reindeers = let
  f x = do
    r <- reindeers
    return $ distancOfReindeerAt r x
  distanceList = map f raceTimeList
  g x = let m = maximum x in map (\y -> if y == m then 1 else 0) x
  in maximum $ map sum $ transpose $ map g distanceList

day14 :: IO ()
day14 = do
  rs <- map inputParser . lines <$> readFile "input14.txt"
  putStrLn ("day14a: " ++ show (maximum $ map (`distancOfReindeerAt` raceTime) rs))
  putStrLn ("day14b: " ++ show (day14b rs))
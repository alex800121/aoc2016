module Day25 where

import Numeric (showBin)
import Data.List (find)

check :: String -> Bool
check ('1' : '0' : xs) = check xs
check [] = True
check _ = False

day25 :: IO ()
day25 = do
  -- input <- readFile "input/input25.txt"
  print $ find (check . (`showBin` "") . (+ (182 * 14))) [0..]

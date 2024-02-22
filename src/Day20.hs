module Day20 where

import Data.List (sort)
import Data.Word (Word32)

type Range a = (a, a)

findLowest :: (Bounded a, Enum a, Ord a) => a -> [Range a] -> a
findLowest start = go start . sort
  where
    go a [] = a
    go a (x@(m, n) : xs)
      | a < m = a
      | a >= m && a <= n = go (succ (snd x)) xs

day20 :: IO ()
day20 = do
  -- input <- readFile "input/input20.txt"
  print (maxBound @Word32)

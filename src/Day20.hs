module Day20 where

import Data.List (sort)
import Data.List.Split
import Data.Word (Word64, Word32)

type ExRange a = (a, a)

findLowest :: (Ord a) => a -> [ExRange a] -> a
findLowest start = go start . sort
  where
    go a [] = a
    go a (x@(m, n) : xs)
      | a < m = a
      | a >= m && a < n = go (snd x) xs
      | otherwise = go a xs

mergeRange :: (Ord a) => [ExRange a] -> [ExRange a]
mergeRange xs = case sort xs of
  [] -> []
  (x : xs) -> go x xs
  where
    go x [] = [x]
    go x (y : ys)
      | snd x >= snd y = go x ys
      | snd x >= fst y = go (fst x, snd y) ys
      | otherwise = x : go y ys

readInput :: String -> ExRange Word64
readInput s | [x, y] <- splitOn "-" s = (read x, read y + 1)

day20 :: IO ()
day20 = do
  input <- map readInput . lines <$> readFile "input/input20.txt"
  print $ findLowest minBound input
  print $ ((fromIntegral (maxBound @Word32) + 1) -) $ sum $ map (uncurry subtract) $ mergeRange input

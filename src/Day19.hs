module Day19 where

import Paths_AOC2016
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq

input = 3004953 :: Int

initSeq = Seq.fromList [1 .. input]

type Circle = (Int, Seq Int)

day19a :: Seq Int -> Int
day19a (x :<| y :<| xs) = day19a (xs |> x)
day19a (x :<| Empty) = x

day19b :: Seq Int -> Int
day19b (x :<| Empty) = x
day19b (x :<| xs) = day19b (xs' |> x)
  where
    l = Seq.length xs
    xs' = Seq.deleteAt (((l + 1) `div` 2) - 1) xs

day19 :: IO ()
day19 = do
  -- input <- (getDataDir >>= readFile . (++ "/input/input19.txt"))
  -- print $ day19b (Seq.fromList [1 .. 5])
  print $ day19a initSeq
  print $ day19b initSeq

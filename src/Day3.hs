module Day3 where
import Data.List (sort, transpose)
import Data.List.Split (chunksOf)

day3 :: IO ()
day3 = do
  input <- map (map (read @Int) . words) . lines <$> readFile "input/input3.txt"
  print $ length $ filter (((>) <$> ((+) <$> head <*> (!! 1)) <*> (!! 2)) . sort) input
  print $ length $ filter (((>) <$> ((+) <$> head <*> (!! 1)) <*> (!! 2)) . sort) $ concatMap (chunksOf 3) $ transpose input

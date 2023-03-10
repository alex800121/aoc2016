module Day17 (day17) where

import MyLib
import Data.List
import Data.Function (on)

day17 :: IO ()
day17 = do
  buckets <- map (read @Int) . lines <$> readFile "input17.txt"
  let ways = sumVariants 150 buckets
      m = minimum $ map length ways
  putStrLn ("day17a: " ++ show (length ways))
  putStrLn ("day17b: " ++ show (length $ filter ((== m) . length) ways))
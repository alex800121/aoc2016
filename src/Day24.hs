
module Day24 (day24) where

import MyLib
import Data.List
import Debug.Trace
import Data.Function (on)
import Control.Applicative

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

toTarget :: Int -> [Int] -> [[Int]]
toTarget t l
  -- | trace (show (t, l)) False = undefined
  | t < 0 = []
  | t == 0 = [[]]
  | t > 0 = do
  y : ys <- safeInit $ tails l
  let xs' =  toTarget (t - y) ys
  if null xs' then empty else return (y : minimum' xs')

minimum' :: [[Int]] -> [Int]
minimum' = minimumBy (\a b -> (compare `on` length) a b <> (compare `on` product) a b)

day24 :: IO ()
day24 = do
  input <- sortBy (flip compare) . map (read @Int) . lines <$> readFile "input24.txt"
  let s = sum input
      t = s `div` 3
      t' = s `div` 4
  -- print (drop 1 $ inits input, s, t)
  putStrLn $ ("day24a: " ++) $ show $ product $ minimum' $ toTarget t input
  putStrLn $ ("day24b: " ++) $ show  $ product $ minimum' $ toTarget t' input

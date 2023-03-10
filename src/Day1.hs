module Day1 (day1) where

import Data.List
import MyLib

day1b :: String -> Int -> Int
day1b _ (-1) = 0
day1b ('(' : xs) n = 1 + day1b xs (n + 1)
day1b (')' : xs) n = 1 + day1b xs (n - 1)
day1b _ _ = undefined

day1 :: IO ()
day1 = do
  floors <- readFile "input1.txt"
  putStrLn ("day1a: " ++ show (uncurry (-) . mapFirst length . fmap length $ partition (== '(') floors))
  putStrLn ("day1b: " ++ show (day1b floors 0))
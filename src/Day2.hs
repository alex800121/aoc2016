module Day2 (day2) where

import Data.List.Split
import Data.List (sort)

inputParser :: String -> [Int]
inputParser = map read . splitOn "x"

wrapperArea :: [Int] -> Int
wrapperArea (x : y : z : _) = let
  a = x * y
  b = y * z
  c = z * x
  d = minimum [a, b, c]
  in 2 * a + 2 * b + 2 * c + d

ribbonLength :: [Int] -> Int
ribbonLength l = let
  (x : y : z : _) = sort l
  in 2 * (x + y) + x * y * z

day2 :: IO ()
day2 = do
  input <- map inputParser . lines <$> readFile "input2.txt"
  putStrLn ("day2a: " ++ show (sum $ map wrapperArea input))  
  putStrLn ("day2b: " ++ show (sum $ map ribbonLength input))
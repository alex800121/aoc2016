module Day5 (day5) where

import MyLib
import Data.List
import Data.List.Split
import Data.Function

threeVowels :: String -> Bool
threeVowels = (>= 3) . length . filter (`elem` "aeiou")

repeatChar :: String -> Bool
repeatChar [] = False
repeatChar [_] = False
repeatChar (x : y : xs) = x == y || repeatChar (y : xs)

disallowedStrings :: [String]
disallowedStrings = ["ab", "cd", "pq", "xy"]

noDisallowed :: String -> Bool
noDisallowed s = not $ any (`isInfixOf` s) disallowedStrings

isNice :: String -> Bool
isNice = (`all` [threeVowels, repeatChar, noDisallowed]) . (&)

isSymmetrical :: String -> Bool
isSymmetrical = reverse >>= (==)

hasSymmetrics :: String -> Bool
hasSymmetrics = any isSymmetrical . divvy 3 1

hasRepeatPair :: String -> Bool
hasRepeatPair s = let
  s2 = divvy 2 1 s
  f [] = False
  f [x] = False
  f [x, y] = False
  f (x : y : xs) = x `elem` xs || f (y : xs)
  in f s2

day5 :: IO ()
day5 = do
  input <- lines <$> readFile "input5.txt"
  putStrLn ("day5a: " ++ show (length $ filter isNice input))
  putStrLn ("day5b: " ++ show (length $ filter ((&&) <$> hasSymmetrics <*> hasRepeatPair) input))
module Day11 (day11) where

import MyLib
import Data.List
import Data.List.Split

input :: String 
input = "hxbxwxba"

step :: String -> String
step = snd . foldr (\x (b, acc) -> case (b, x) of
    (False, _) -> (False, x : acc)
    (True, 'z') -> (True, 'a' : acc)
    (True, _) -> (False, succ x : acc)) (True, [])

inputList :: [String]
inputList = iterate step input

has3Increase :: String -> Bool
has3Increase s = let
  s' = divvy 3 1 s
  f [x, y, z] = y == succ x && z == succ y
  in any f s'

hasNoIOL :: String -> Bool
hasNoIOL = all (`notElem` "iol")

has2Repeats :: String -> Bool
has2Repeats s = let
  s' = filter ((>= 2) . length) $ group s
  in length s' >= 2

day11 :: IO ()
day11 = do
  let day11a = dropWhile (\x -> not $ all ($ x) [has3Increase, hasNoIOL, has2Repeats]) inputList
      day11b = dropWhile (\x -> not $ all ($ x) [has3Increase, hasNoIOL, has2Repeats]) $ tail day11a
  putStrLn ("day11a: " ++ head day11a)
  putStrLn ("day11b: " ++ head day11b)
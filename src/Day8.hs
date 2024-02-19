module Day8 where

import Data.Array
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import MyLib
import Data.Bifunctor (Bifunctor(..))
import Debug.Trace (traceShow)
import Data.List (foldl')

type Rect = Array Index Bool

type Index = (Int, Int)

initRect = listArray ((0, 0), (49, 5)) (repeat False)
testRect = listArray ((0, 0), (6, 2)) (repeat False)

printRect = unlines . drawGraph (\case Just True -> '#'; _ -> ' ') . Map.fromList . assocs

readIns :: String -> Rect -> Rect
readIns s r = case words s of
  ["rect", xy]
    | [x, y] <- map (read @Int) (splitOn "x" xy) ->
        r // [(i, True) | i <- range ((0, 0), (x - 1, y - 1))]
  [_, "row", y, _, n] | y <- read @Int (drop 2 y), n <- read @Int n -> 
    array 
      b
      [(i, r ! a i)
        | i <- range b,
          let a = if snd i == y then first ((`mod` lx) . (subtract n)) else id]
  [_, "column", x, _, n] | x <- read @Int (drop 2 x), n <- read @Int n ->
    array 
      b
      [(i, r ! a i)
        | i <- range b,
          let a = if fst i == x then second ((`mod` ly) . (subtract n)) else id]
  where
    b = bounds r
    (lx, ly) = (fst (snd b) - fst (fst b) + 1, snd (snd b) - snd (fst b) + 1)

day8 :: IO ()
day8 = do
  input <- lines <$> readFile "input/input8.txt"
  let a = printRect $ foldl' (flip readIns) initRect input
  print $ length $ filter (== '#') a
  putStrLn a

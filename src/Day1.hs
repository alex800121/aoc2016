module Day1 where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.Ix (Ix (range))
import Data.List (scanl')
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import MyLib (Direction (..), firstRepeat', toIndex, (+&))

type Index = (Int, Int)

type Ins = (Char, Int)

type Pos = (Direction, Index)

toTurn :: Char -> Direction -> Direction
toTurn 'R' = succ
toTurn 'L' = pred

turn :: Char -> Pos -> Pos
turn = first . toTurn

forward :: Int -> Pos -> Pos
forward n p = second (+& bimap (* n) (* n) (toIndex (fst p))) p

readIns :: Ins -> Pos -> Pos
readIns = (.) <$> forward . snd <*> turn . fst

allLoc :: [Index] -> [Index]
allLoc (x : y : xs) = tail (f $ range (m, n)) <> allLoc (y : xs)
  where
    m = min x y
    n = max x y
    f = if x > y then reverse else id

day1 :: IO ()
day1 = do
  input <- map ((,) <$> head <*> read @Int . tail) . splitOn ", " <$> readFile "input/input1.txt"
  print
    . uncurry ((+) `on` abs)
    . snd
    $ foldl' (flip readIns) (North, (0, 0)) input
  print
    . fmap (uncurry ((+) `on` abs) . snd)
    . firstRepeat'
    . ((:) <$> head <*> allLoc)
    . map snd
    $ scanl' (flip readIns) (North, (0, 0)) input

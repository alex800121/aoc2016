module Day2 where

import Data.Array.Unboxed
import Data.List
import MyLib
import Control.Monad.Trans.State.Strict (State, put)
import Control.Monad.Trans.State.Strict as S
import Data.Char (intToDigit)
import Control.Monad (forM)

type Pad = Array Index Char

type Index = (Int, Int)

pad :: Pad
pad =
  array
    b
    [ (i, intToDigit $ 1 + (3 * snd i) + fst i)
      | i <- range b
    ]
  where
    b = ((0, 0), (2, 2))

pad' :: Pad
pad' =
  listArray
    b
    [' ', ' ', '5', ' ', ' ',
     ' ', '2', '6', 'A', ' ',
     '1', '3', '7', 'B', 'D',
     ' ', '4', '8', 'C', ' ',
     ' ', ' ', '9', ' ', ' '
    ]
  where
    b = ((0, 0), (4, 4))

test = "ULL\nRRDDD\nLURDL\nUUUUD"

readIns :: Pad -> Direction -> Index -> Index
readIns a d i
  | inRange b i' && x' /= ' ' = i'
  | otherwise = i
  where
    b = bounds a
    i' = i +& toIndex d
    x' = a ! i'

fromChar :: Char -> Direction
fromChar 'U' = North
fromChar 'D' = South
fromChar 'L' = West
fromChar 'R' = East

day2 :: IO ()
day2 = do
  input <- map (map fromChar) . lines <$> readFile "input/input2.txt"
  putStrLn $ map (pad !) $ tail $ scanl' (foldl' (flip (readIns pad))) (1, 1) input
  putStrLn $ map (pad' !) $ tail $ scanl' (foldl' (flip (readIns pad'))) (2, 2) input

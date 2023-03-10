module Day3 (day3) where

import MyLib
import Data.Set (Set, size, fromList, insert, union, empty)

type Index = (Int, Int)

deliverList :: Index -> String -> [Index]
deliverList i "" = [i]
deliverList i (x : xs) = i : deliverList (i +& dir) xs
  where
    dir = case x of
      '^' -> (0, 1)
      '>' -> (1, 0)
      'v' -> (0, -1)
      '<' -> (-1, 0)
      _ -> undefined

deliverListb :: (Index, Index) -> String -> (Set Index, Set Index) -> Set Index
deliverListb (i1, i2) "" (s1, s2) = union (insert i1 s1) $ insert i2 s2
deliverListb (i1, i2) (x : xs) (s1, s2) = deliverListb (i2, i1 +& dir) xs (s2, insert i1 s1)
  where
    dir = case x of
      '^' -> (0, 1)
      '>' -> (1, 0)
      'v' -> (0, -1)
      '<' -> (-1, 0)
      _ -> undefined

day3 :: IO ()
day3 = do
  input <- readFile "input3.txt"
  putStrLn ("day3a: " ++ show (size $ fromList $ deliverList (0, 0) input))
  putStrLn ("day3b: " ++ show (size $ deliverListb ((0, 0), (0, 0)) input (empty, empty)))
module Day15 where

import MyLib (emcd)
import Data.List (foldl1')

-- >>> emcd 5 23
-- (1,-9,2)

data Disc = Disc
  { _layer :: !Int,
    _positions :: !Int,
    _time0 :: !Int
  }
  deriving (Show, Eq, Ord)

-- Disc #1 has 13 positions; at time=0, it is at position 10.

-- alignDisc :: Disc -> Disc -> Disc
alignDisc (Disc l0 p0 t0) (Disc l1 p1 t1) = Disc 0 p (negate d `mod` p)
  where
    (a, b, c) = emcd p0 p1 -- a  == p0 * b  + p1 * c
    a' = (p1 - t1 - l1) - (p0 - t0 - l0) -- a' == p0 * b' + p1 * c'
    b' = (a' `div` a) * b -- t == p0 * b' + (p0 - t0 - l0)
    c' = (a' `div` a) * c -- t == p1 * c' + (p1 - t1 - l1)
    p = lcm p0 p1
    d = p0 * b' + (p0 - t0 - l0)

-- >>> alignDisc (Disc 1 13 10) (Disc 2 17 15)
-- (-2,-8,6)
-- >>> 13 * (-8) + (13 - 10 - 1)
-- -102
-- >>> 17 * (-6) + (17 - 15 - 2)
-- -102
-- >>> lcm 13 17
-- 221
-- Disc {_layer = 0, _positions = 221, _time0 = 102}

readDisc :: String -> Disc
readDisc s
  | [_, l, _, p, _, _, _, _, _, _, _, t] <- words s =
      Disc (read (tail l)) (read p) (read (init t))

pressTime :: Disc -> Int
pressTime (Disc a b c) = (b - c - a) `mod` b

day15 :: IO ()
day15 = do
  input <- map readDisc . lines <$> readFile "input/input15.txt"
  -- print $ alignDisc (input !! 1) (head input)
  print $ pressTime $ foldr1 alignDisc input
  print $ pressTime $ foldr alignDisc (Disc (maximum (map _layer input) + 1) 11 0) input

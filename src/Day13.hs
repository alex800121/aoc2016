module Day13 where

import Data.Bits (popCount)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib ((+&))

input = 1358

type Index = (Int, Int)

calcOpen :: Int -> Index -> Bool
calcOpen n (x, y) = x >= 0 && y >= 0 && even (popCount a)
  where
    a = x * x + 3 * x + 2 * x * y + y + y * y + n

bfs :: Int -> (Int -> Index -> Bool) -> (Index -> Set Index) -> Set Index -> Set Index -> (Int, Set Index)
bfs n ended nextF visited next
  | any (ended n) next = (n, visited')
  | otherwise = bfs (n + 1) ended nextF visited' next'
  where
    visited' = visited `Set.union` next
    next' = Set.unions (Set.map nextF next) Set.\\ visited'

nextF :: Int -> Index -> Set Index
nextF n i = Set.fromList $ filter (calcOpen n) $ map (+& i) adjacent

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

start = (1, 1)

end = (31, 39)

day13 :: IO ()
day13 = do
  -- input <- readFile "input/input13.txt"
  print $ fst $ bfs 0 (const (== end)) (nextF input) Set.empty (Set.singleton start)
  print $ length $ snd $ bfs 0 (\x _ -> x == 50) (nextF input) Set.empty (Set.singleton start)

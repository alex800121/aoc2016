module Day24 where

import Data.Char (digitToInt, isDigit)
import Data.List (partition, tails, uncons, permutations)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import MyLib

data S
  = Space
  | Point Int
  deriving (Show, Eq, Ord)

type Index = (Int, Int)

type Edge = (P, P)

type P = (Index, Int)

buildMap :: Map Index S -> Map Edge Int
buildMap m = Map.unions $ map (uncurry (bfs m)) ps
  where
    p = mapMaybe (\(x, y) -> case y of Space -> Nothing; Point i -> Just (x, i)) $ Map.toList m
    ps = mapMaybe uncons $ tails p

bfs :: Map Index S -> P -> [P] -> Map Edge Int
bfs m p end = go (Set.singleton (fst p)) end Set.empty 0 Map.empty
  where
    go _ [] _ _ acc = acc
    go start ends visited n acc = go start' ends' visited' (n + 1) acc'
      where
        (finished, ends') = partition ((`Set.member` start) . fst) ends
        start' =
          Set.filter ((&&) <$> (`Set.notMember` visited') <*> (`Map.member` m))
            . Set.unions
            $ map (\x -> Set.map (x +&) start) adjacent
        visited' = Set.union visited start
        acc' = Map.union acc $ Map.fromList [((p, b), n) | b <- finished]

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

getDistance :: Map Edge Int -> Edge -> Int
getDistance m e = fromMaybe (m Map.! swap e) (m Map.!? e)

day24 :: IO ()
day24 = do
  input <- drawMap (\case '.' -> Just Space; x | isDigit x -> Just $ Point (digitToInt x); _ -> Nothing) . lines <$> readFile "input/input24.txt"
  let p = mapMaybe (\(x, y) -> case y of Space -> Nothing; Point i -> Just (x, i)) $ Map.toList input
      starts = mapMaybe uncons $ filter ((== 0) . snd . head) $ permutations p
      m = buildMap input
  print m

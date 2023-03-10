module Day9 (day9) where

import MyLib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function
import Data.Foldable
import Data.List (nub, insert, permutations)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Carrier.State.Strict
import Control.Effect.State
import Debug.Trace
import Data.Maybe (fromMaybe)
import Control.Monad (zipWithM)

type MyMap = Map String (Map String Int)
data Q risk node = Q {risk :: risk, path :: [node], riskMap :: Map node (Map node risk)} deriving (Show, Eq)
instance (Ord risk, Ord node) => Ord (Q risk node) where
  compare (Q r1 q1 _) (Q r2 q2 _)= compare r1 r2 <> (compare `on` length) q2 q1

type MyQ = [Q Int String]

inputParser :: String -> MyMap
inputParser = (\(x : _ : y : _ : z : _) ->
    Map.fromList [(x, Map.singleton y (read z)), (y, Map.singleton x (read z))]) . words

getAllPaths :: MyMap -> [[String]]
getAllPaths = permutations . Map.keys

calcPathLen :: MyMap -> [String] -> Maybe Int
calcPathLen m l = sum <$> zipWithM (\x y -> Map.lookup x m >>= Map.lookup y) l (tail l)

initQ :: MyMap -> MyQ
initQ myMap = let
  starting = nub $ Map.keys myMap
  in map (\x -> Q 0 [x] myMap) starting

nextQ :: (Ord risk, Num risk, Ord node) => [Q risk node] -> [Q risk node]
nextQ [] = error "empty Q in next"
nextQ ((Q risk (x : xs) m) : q) = let
  nextStopWithRisk = Map.toList $ fromMaybe Map.empty $ Map.lookup x m
  f (nextStop, nextRisk) = Q (risk + nextRisk) (nextStop : x : xs) (Map.adjust (Map.delete x) nextStop $ Map.delete x m)
  in foldr (insert . f) q nextStopWithRisk

runQUntil :: (Ord risk, Num risk, Ord node) => (Q risk node -> Bool) -> [Q risk node] -> Q risk node
runQUntil _ [] = error "empty Q when running"
runQUntil _ [x] = x
runQUntil cond q'@(q : _) = if cond q then q else runQUntil cond (nextQ q')

day9 :: IO ()
day9 = do
  myMap <- Map.unionsWith Map.union . map inputParser . lines <$> readFile "input9.txt"
  let
    locations = Map.keys myMap
    riskList = filter (/= Nothing) $ map (calcPathLen myMap) $ getAllPaths myMap
  -- putStrLn ("day9a: " ++ show ((.risk) . runQUntil ((== length locations) . length . (.path)) $ initQ myMap))
  putStrLn ("day9a: " ++ show (minimum riskList))
  putStrLn ("day9b: " ++ show (maximum riskList))
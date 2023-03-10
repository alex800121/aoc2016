module Day18 (day18) where

import MyLib
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (forM_)
import Control.Effect.State
import Control.Carrier.State.Strict

type Index = (Int, Int)
type LightMap = Map Index Char

inputParser :: [String] -> Map Index Char
inputParser = drawMap Just

drawLightMap :: LightMap -> String
drawLightMap = unlines . drawGraph f
  where
    f Nothing = ' '
    f (Just a) = a

surrounds :: [Index]
surrounds = delete (0, 0) [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]

step' :: LightMap -> LightMap
step' m = Map.mapWithKey f m
  where
    ks = Map.keys m
    minX = minimum $ map fst ks
    maxX = maximum $ map fst ks
    minY = minimum $ map snd ks
    maxY = maximum $ map snd ks
    corners = [(x, y) | x <- [minX, maxX], y <- [minY, maxY]]
    f k a
      | k `elem` corners = '#'
      | a == '#' = if onN == 2 || onN == 3 then '#' else '.'
      | a == '.' = if onN == 3 then '#' else '.'
      where
        sur = map ((\x -> Map.findWithDefault '.' x m) . (+& k)) surrounds
        onN = length (filter (== '#') sur)
        offN = length (filter (== '.') sur)

step :: LightMap -> LightMap
step m = Map.mapWithKey f m
  where
    f k a
      | a == '#' = if onN == 2 || onN == 3 then '#' else '.'
      | a == '.' = if onN == 3 then '#' else '.'
      where
        sur = map ((\x -> Map.findWithDefault '.' x m) . (+& k)) surrounds
        onN = length (filter (== '#') sur)
        offN = length (filter (== '.') sur)

runLM :: (LightMap -> LightMap) -> Int -> LightMap -> LightMap
runLM f n m = run . execState m $ forM_ [1..n] $ \_ -> modify f

day18 :: IO ()
day18 = do
  input <- inputParser . lines <$> readFile "input18.txt"
  let
    ks = Map.keys input
    minX = minimum $ map fst ks
    maxX = maximum $ map fst ks
    minY = minimum $ map snd ks
    maxY = maximum $ map snd ks
    corners = [(x, y) | x <- [minX, maxX], y <- [minY, maxY]]
    input' = foldr (`Map.insert` '#') input corners
  putStrLn ("day18a: " ++ show (length $ Map.filter (== '#') $ runLM step 100 input))
  putStrLn ("day18b: " ++ show (length $ Map.filter (== '#') $ runLM step' 100 input'))
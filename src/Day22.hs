module Day22 where

import Paths_AOC2016
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char

type FS = Map Index Node

type Index = (Int, Int)

data Node
  = Available
  | Full
  | Blocked
  deriving (Show, Eq, Ord)

fsParser :: Parser FS
fsParser = do
  string "/dev/grid/node-x"
  x <- signedInteger
  string "-y"
  y <- signedInteger
  space
  a <- signedInteger <* char 'T'
  space
  b <- signedInteger <* char 'T'
  space
  c <- signedInteger <* char 'T'
  takeRest
  pure $ Map.singleton (x, y) (f a b c)
  where
    f m n o
      | m > 100 = Blocked
      | n > o = Full
      | otherwise = Available

toChar Blocked = '#'
toChar Available = ' '
toChar Full = '.'

minX = 0

maxX = 34

minY = 0

maxY = 26

target = (maxX, 0)

day22 :: IO ()
day22 = do
  input <- Map.unions . mapMaybe (parseMaybe fsParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  print $ length $ Map.filter (== Full) input
  let Just avail = fmap fst $ find ((== Available) . snd) $ Map.toList input
      b = fst avail + snd avail + fst target + 5 * (fst target - 1)
  -- mapM_ putStrLn $ drawGraph (maybe ' ' toChar) input
  -- print avail
  print b

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Day17 where

import Paths_AOC2016
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt)
import Data.Foldable (find, minimumBy, maximumBy)
import Data.Ix (Ix (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Debug.Trace
import MyLib (Direction (..), toIndex, (+&))
import "cryptohash-md5" Crypto.Hash.MD5
import Data.Function (on)

input = "vwbaicqe"
-- input = "kglvqrro"

mapRange = ((0, 0), (3, 3))

type Path = (Index, ByteString)

type Index = (Int, Int)

type Range = (Index, Index)

withPath :: ByteString -> ByteString -> ByteString
withPath input path = encode $ hash $ input <> path

decodeDoor :: ByteString -> [Direction]
decodeDoor s =
  map snd $
    filter (f . C.index s . fromEnum . fst) [(0, North), (2, West), (3, East), (1, South)]
  where
    f = (> 10) . digitToInt

dirToChar :: Direction -> Char
dirToChar North = 'U'
dirToChar South = 'D'
dirToChar West = 'L'
dirToChar East = 'R'

step :: Range -> ByteString -> Path -> [Path]
step b input (i, path) = filter (inRange b . fst) $ map (\d -> (i +& toIndex d, path `C.snoc` dirToChar d)) ds
  where
    ds = decodeDoor $ withPath input path

bfs :: Range -> ByteString -> Index -> Set Path -> Set ByteString -> Set ByteString
bfs b input end start acc
  -- | traceShow start False = undefined
  | Set.null start = acc
  | otherwise = bfs b input end start' acc'
  where
    (x, y) = Set.partition ((== end) . fst) start
    start' = Set.unions $ Set.map (Set.fromList . step b input) y
    acc' = Set.union acc $ Set.map snd x

day17 :: IO ()
day17 = do
  -- input <- (getDataDir >>= readFile . (++ "/input/input17.txt"))
  let a = bfs mapRange input (3, 3) (Set.singleton ((0, 0), "")) Set.empty
  C.putStrLn $ minimumBy (compare `on` B.length) a
  print $ maximum $ Set.map B.length a

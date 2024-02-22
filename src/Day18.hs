module Day18 where

import Data.Bits (Bits (..), testBit)
import Data.Function (on)
import Data.List (elemIndices, findIndices)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import Data.WideWord.Word128
import MyLib (firstCycle')

fromS :: String -> Word128
fromS = foldr (flip setBit) 0 . elemIndices '^'

toS :: Int -> Word128 -> String
toS i s = map (\i -> if testBit s i then '^' else '.') [0 .. i - 1]

step :: Int -> Word128 -> Word128
step i s = sl `xor` sr
  where
    sl = clearBit (s `shiftL` 1) i
    sr = s `shiftR` 1

day18 :: IO ()
day18 = do
  input <- init <$> readFile "input/input18.txt"
  let l = iterate (step 100) $ fromS input
  print $ sum $ map ((100 -) . popCount) $ take 40 l
  print $ sum $ map ((100 -) . popCount) $ take 400000 l

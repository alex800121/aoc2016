module Day21 where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Foldable (Foldable(..))
import Data.List (scanl')

readIns :: String -> Seq Char -> Seq Char
readIns ins s = case words ins of
  ["swap", "position", x, _, _, y] ->
    let a = read x
        b = read y
        ca = Seq.index s a
        cb = Seq.index s b
     in Seq.update a cb $ Seq.update b ca s
  ["swap", "letter", x, _, _, y] ->
    let Just a = Seq.elemIndexL ca s
        Just b = Seq.elemIndexL cb s
        ca = head x
        cb = head y
     in Seq.update a cb $ Seq.update b ca s
  ["rotate", "left", x, _] -> uncurry (flip (Seq.><)) $ Seq.splitAt (read x `mod` l) s
  ["rotate", "right", x, _] -> uncurry (flip (Seq.><)) $ Seq.splitAt (negate (read x) `mod` l) s
  ["rotate", "based", _, _, _, _, x] ->
    let Just a = Seq.elemIndexL ca s
        ca = head x
        n = if a >= 4 then a + 2 else a + 1
     in uncurry (flip (Seq.><)) $ Seq.splitAt (negate n `mod` l) s
  ["reverse", _, x, _, y] ->
    let (a', c) = Seq.splitAt (read y + 1) s
        (a, b) = Seq.splitAt (read x) a'
     in a Seq.>< Seq.reverse b Seq.>< c
  ["move", _, x, _, _, y] -> Seq.insertAt (read y) (Seq.index s (read x)) $ Seq.deleteAt (read x) s
  where
    l = Seq.length s

day21 :: IO ()
day21 = do
  input <- lines <$> readFile "input/input21.txt"
  print $ foldl' (flip readIns) (Seq.fromList "abcdefgh") input

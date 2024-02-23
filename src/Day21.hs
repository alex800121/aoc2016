module Day21 where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Foldable (Foldable(..))

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
  ["rotate", "left", x, _] -> uncurry (flip (Seq.><)) $ Seq.splitAt (read x) s
  ["rotate", "right", x, _] -> uncurry (flip (Seq.><)) $ Seq.splitAt (Seq.length s - read x) s
  ["rotate", "based", _, _, _, _, x] ->
    let Just a = Seq.elemIndexL ca s
        ca = head x
        n = (if a >= 4 then a + 2 else a + 1) `mod` l
        l = Seq.length s
     in uncurry (flip (Seq.><)) $ Seq.splitAt (l - n) s
  ["reverse", _, x, _, y] ->
    let (a', c) = Seq.splitAt (read y + 1) s
        (a, b) = Seq.splitAt (read x) s
     in a Seq.>< Seq.reverse b Seq.>< c
  ["move", _, x, _, _, y] -> Seq.insertAt (read y) (Seq.index s (read x)) $ Seq.deleteAt (read x) s

day21 :: IO ()
day21 = do
  input <- lines <$> readFile "input/input21.txt"
  -- print
  --   . readIns "rotate based on position of letter d"
  --   . readIns "rotate based on position of letter b"
  --   . readIns "move position 3 to position 0"
  --   . readIns "move position 1 to position 4"
  --   . readIns "rotate left 1 step"
  --   . readIns "reverse positions 0 through 4"
  --   . readIns "swap letter d with letter b"
  --   . readIns "swap position 4 with position 0"
  --   $ Seq.fromList "abcde"
  print $ foldl' (flip readIns) (Seq.fromList "abcdefgh") input

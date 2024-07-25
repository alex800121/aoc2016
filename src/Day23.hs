module Day23 where

import Paths_AOC2016
import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlpha, ord)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Debug.Trace

type Reg = Array Int Int

type Arg = Either Char Int

data Ins
  = Cpy !Arg !Arg
  | Inc !Char
  | Dec !Char
  | Jnz !Arg !Arg
  | Tgl !Char
  deriving (Show, Eq, Ord)

type GameState = (Vector Ins, (Int, Reg))

readReg :: Reg -> Char -> Int
readReg a c = a ! ord c

readArg :: Reg -> Arg -> Int
readArg r = either (readReg r) id

initReg :: Reg
initReg = listArray (ord 'a', ord 'd') (replicate 4 0)

readIns :: String -> Ins
readIns s = case words s of
  ["cpy", x, y] -> Cpy (f x) (f y)
  ["jnz", x, y] -> Jnz (f x) (f y)
  ["inc", x] -> Inc (head x)
  ["dec", x] -> Dec (head x)
  ["tgl", x] -> Tgl (head x)
  where
    f x = if isAlpha (head x) then Left (head x) else Right (read x)

ins :: Ins -> GameState -> GameState
ins (Cpy a (Left c)) g = second (bimap (+ 1) (// [(ord c, readArg (snd $ snd g) a)])) g
ins (Jnz a i) g = second (first (+ if readArg (snd $ snd g) a == 0 then 1 else readArg (snd $ snd g) i)) g
ins (Inc c) g = second (bimap (+ 1) (// [(ord c, readReg (snd $ snd g) c + 1)])) g
ins (Dec c) g = second (bimap (+ 1) (// [(ord c, readReg (snd $ snd g) c - 1)])) g
ins (Tgl c) g
  | V.length (fst g) > i = bimap (V.modify (\v -> M.modify v toggle i)) (first (+ 1)) g
  | otherwise = second (first (+ 1)) g
  where
    i = readReg (snd $ snd g) c + fst (snd g)

toggle :: Ins -> Ins
toggle (Cpy a b) = Jnz a b
toggle (Jnz a b) = Cpy a b
toggle (Inc a) = Dec a
toggle (Dec a) = Inc a
toggle (Tgl a) = Inc a

go :: GameState -> [GameState]
go (v, g) = maybe [(v, g)] (((v, g) :) . go . (`ins` (v, g))) (v V.!? fst g)

day23 :: IO ()
day23 = do
  input <- V.fromList . map readIns . lines <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  -- input <- V.fromList . map readIns . lines <$> readFile "input/test23.txt"
  -- print $ (! ord 'a') $ snd $ snd $ last $ go (input, (0, initReg // [(ord 'a', 7)]))
  print $ product [1..7] + (74 * 79)
  print $ product [1..12] + (74 * 79)

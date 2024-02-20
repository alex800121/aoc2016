module Day12 where

import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlpha, ord)
import Data.Vector (Vector)
import qualified Data.Vector as V

type Reg = Array Int Int

type Arg = Either Char Int

data Ins
  = Cpy !Arg !Char
  | Inc !Char
  | Dec !Char
  | Jnz !Arg !Int
  deriving (Show, Eq, Ord)

type GameState = (Int, Reg)

readReg :: Reg -> Char -> Int
readReg a c = a ! ord c

readArg :: Reg -> Arg -> Int
readArg r = either (readReg r) id

initReg :: Reg
initReg = listArray (ord 'a', ord 'd') (replicate 4 0)

readIns :: String -> Ins
readIns s = case words s of
  ["cpy", x, y] -> Cpy (if isAlpha (head x) then Left (head x) else Right (read x)) (head y)
  ["jnz", x, y] -> Jnz (if isAlpha (head x) then Left (head x) else Right (read x)) (read y)
  ["inc", x] -> Inc (head x)
  ["dec", x] -> Dec (head x)

ins :: Ins -> GameState -> GameState
ins (Cpy a c) g = bimap (+ 1) (// [(ord c, readArg (snd g) a)]) g
ins (Jnz a i) g = first (+ if readArg (snd g) a == 0 then 1 else i) g
ins (Inc c) g = bimap (+ 1) (// [(ord c, readReg (snd g) c + 1)]) g
ins (Dec c) g = bimap (+ 1) (// [(ord c, readReg (snd g) c - 1)]) g

go :: Vector Ins -> GameState -> GameState
go v g = maybe g (go v . (`ins` g)) (v V.!? fst g)

day12 :: IO ()
day12 = do
  input <- V.fromList . map readIns . lines <$> readFile "input/input12.txt"
  print $ go input (0, initReg)
  print $ go input (0, initReg // [(ord 'c', 1)])

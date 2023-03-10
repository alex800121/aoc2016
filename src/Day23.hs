{-# LANGUAGE LambdaCase #-}
module Day23 (day23) where

import MyLib
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import Debug.Trace

data Register = A | B deriving (Show, Eq, Ord)
data Instruction = HLF Register
                 | TPL Register
                 | INC Register
                 | JMP Int
                 | JIE Register Int
                 | JIO Register Int
  deriving (Show, Eq, Ord)
data Machine = M { pointer :: Int, register :: Map Register Int, ins :: Vector Instruction }
  deriving (Show, Eq, Ord)

step :: Machine -> Maybe Machine
step m = (`interpret` m) <$> m.ins Vector.!? m.pointer 

run :: Machine -> Int
-- run m = trace (show (m.pointer, m.register)) $ case step m of
run m = case step m of
  Nothing -> m.register Map.! B
  Just m' -> run m'


interpret :: Instruction -> Machine -> Machine
interpret (HLF p) m = m { pointer = m.pointer + 1, register = Map.adjust (`div` 2) p m.register }
interpret (TPL p) m = m { pointer = m.pointer + 1, register = Map.adjust (* 3) p m.register }
interpret (INC p) m = m { pointer = m.pointer + 1, register = Map.adjust (+ 1) p m.register }
interpret (JMP n) m = m { pointer = m.pointer + n }
interpret (JIE p i) m = m { pointer = if even (m.register Map.! p) then m.pointer + i else m.pointer + 1 }
interpret (JIO p i) m = m { pointer = if (m.register Map.! p) == 1 then m.pointer + i else m.pointer + 1 }

inputParser :: String -> Instruction
inputParser s = case x of
  "hlf" -> HLF $ (\case ; "a" -> A ; _ -> B) $ head xs
  "tpl" -> TPL $ (\case ; "a" -> A ; _ -> B) $ head xs
  "inc" -> INC $ (\case ; "a" -> A ; _ -> B) $ head xs
  "jmp" -> let y : ys = head xs in JMP $ if y == '-' then negate (read ys) else read ys
  "jie" -> let (y : _) : (z : zs) : _ = xs in JIE (if y == 'a' then A else B) $ if z == '-' then negate (read zs) else read zs
  "jio" -> let (y : _) : (z : zs) : _ = xs in JIO (if y == 'a' then A else B) $ if z == '-' then negate (read zs) else read zs
  where
    x : xs = words s

day23 :: IO ()
day23 = do
  input <- Vector.fromList . map inputParser . lines <$> readFile "input23.txt"
  let initMachine = M 0 (Map.fromList [(A, 0), (B, 0)]) input
      initMachine2 = M 0 (Map.fromList [(A, 1), (B, 0)]) input
  putStrLn $ ("day22a: " ++) $ show $ run initMachine
  putStrLn $ ("day22b: " ++) $ show $ run initMachine2

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Day7 (day7) where

import MyLib
import Data.Bits
import GHC.Word
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec (many, anySingleBut, anySingle, eof, parseMaybe, parseTest, try)
import Text.Megaparsec.Char.Lexer ()
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Char
import Control.Carrier.State.Strict
import Control.Effect.State

type Wire = String
data Unit a = I a | W Wire deriving (Show, Eq, Ord)
data Input a = Input a | Circuit (Circuit a) deriving (Show, Eq, Ord)
data BiOp = AND | OR | RSHIFT | LSHIFT deriving (Show, Eq, Ord, Read)
data Op a = ID a
          | NOT a
          | BiOp BiOp a a
  deriving (Show, Eq, Ord)
type CircuitMap a = Map Wire (Op (Unit a))
data Circuit a = C Wire (Op (Input a)) deriving (Show, Eq, Ord)  
type Cache a = Map Wire a
unitParser :: Parser (Unit Word16)
unitParser = do
  (I . fromIntegral <$> signedInteger) <|> (W <$> many (anySingleBut ' '))

notParser :: Parser (CircuitMap Word16)
notParser = do
  string "NOT" <* space
  y <- unitParser <* space
  string "->" <* space
  z <- many anySingle <* eof
  return (Map.singleton z (NOT y))

idParser :: Parser (CircuitMap Word16)
idParser = do
  y <- unitParser <* space
  string "->" <* space
  z <- many anySingle <* eof
  return (Map.singleton z (ID y))

biOpParser :: Parser (CircuitMap Word16)
biOpParser = do
  x <- unitParser <* space
  biop <- read @BiOp <$> many (anySingleBut ' ') <* space
  y <- unitParser <* space
  string "->" <* space
  z <- many anySingle <* eof
  return (Map.singleton z (BiOp biop x y))

inputParser :: Parser (CircuitMap Word16)
inputParser = try notParser <|> try idParser <|> try biOpParser

circuitFromWire :: CircuitMap Word16 -> Wire -> Circuit Word16
circuitFromWire cm w = C w $ case cm Map.! w of
  ID x -> ID (unit x)
  NOT x -> NOT (unit x)
  BiOp o x y -> BiOp o (unit x) (unit y)
  where
    unit (I i) = Input i
    unit (W w) = Circuit (circuitFromWire cm w)

calcInput :: (Integral a, Bits a, Has (State (Cache a)) sig m) => Input a -> m a
calcInput (Input x) = return x
calcInput (Circuit c) = calcCircuit c

calcCircuit:: (Integral a, Bits a, Has (State (Cache a)) sig m) => Circuit a -> m a
calcCircuit (C w op) = do
  m <- get
  case Map.lookup w m of
    Just x -> return x
    Nothing -> case op of
      ID x -> calcInput x >>= \z -> modify (Map.insert w z) >> return z
      NOT x -> complement <$> calcInput x >>= \z -> modify (Map.insert w z) >> return z
      BiOp AND x y -> (.&.) <$> calcInput x <*> calcInput y >>= \z -> modify (Map.insert w z) >> return z
      BiOp OR x y -> (.|.) <$> calcInput x <*> calcInput y >>= \z -> modify (Map.insert w z) >> return z
      BiOp RSHIFT x y -> shiftR <$> calcInput x <*> (fromIntegral <$> calcInput y) >>= \z -> modify (Map.insert w z) >> return z
      BiOp LSHIFT x y -> shiftL <$> calcInput x <*> (fromIntegral <$> calcInput y) >>= \z -> modify (Map.insert w z) >> return z

getOpContent :: Op a -> [a]
getOpContent (ID x) = [x]
getOpContent (NOT x) = [x]
getOpContent (BiOp _ y x) = [y, x]

day7 :: IO ()
day7 = do
  x <- Map.unions . map (fromJust . parseMaybe inputParser) . lines <$> readFile "input7.txt"
  let a = run . evalState (Map.empty :: Cache Word16) $ calcCircuit (circuitFromWire x "a")
      x' = Map.insert "b"  (ID (I a)) x
  -- x <- Map.unions . map (fromJust . parseMaybe inputParser) . lines <$> readFile "test7.txt"
  putStrLn ("day7a: " ++ show a)
  putStrLn ("day7b: " ++ show (run . evalState (Map.singleton "b" a) $ calcCircuit (circuitFromWire x' "a")))
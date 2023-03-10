module Day16 (day16) where

import MyLib
import Data.Function ((&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec ( parseMaybe, anySingle, anySingleBut, many, eof )
import Text.Megaparsec.Char ( char, eol, space, string )
import Data.List.Split
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

type Sue = IntMap Hint
type Hint = Map String Int
type Hint' = Map String (Int -> Int)

hintParser :: Parser Hint
hintParser = Map.singleton <$> (many (anySingleBut ':') <* string ": ") <*> signedInteger

inputParser :: Parser Sue
inputParser = do
  a <- string "Sue " >> signedInteger <* string ": "
  let
    f = do
      x <- hintParser
      (string ", " >> (x :) <$> f) <|> (eof >> return [x])
  b <- Map.unions <$> f
  return $ IntMap.singleton a b


day16 :: IO ()
day16 = do
  hints <- Map.unions . map (fromJust . parseMaybe hintParser) . lines <$> readFile "input16-1.txt"
  sues <- IntMap.unions . map (fromJust . parseMaybe inputParser) . lines <$> readFile "input16.txt"
  let
    hints' = Map.mapWithKey (\k a -> case k of
      "cats" -> (> a)
      "trees" -> (> a)
      "pomeranians" -> (< a)
      "goldfish" -> (< a)
      _ -> (== a)) hints
  putStrLn ("day16a: " ++ show (IntMap.filter (`Map.isSubmapOf` hints) sues))
  putStrLn ("day16b: " ++ show (IntMap.filter (flip (Map.isSubmapOfBy (&)) hints') sues))

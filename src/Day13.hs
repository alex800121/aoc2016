module Day13 (day13) where

import MyLib
import Data.List.Split (divvy)
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec (MonadParsec(eof), many, parseMaybe, anySingle, anySingleBut)
import Control.Applicative ((<|>))
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (space, char, eol, string)
import Data.Maybe (fromJust)

type HappyMap = Map String (Map String Int)
type PairMap = Map [String] Int

inputParser :: Parser HappyMap
inputParser = do
  a <- many (anySingleBut ' ') <* space
  string "would" <* space
  b <- ((string "gain" >> return id) <|> (string "lose" >> return negate)) <* space
  i <- signedInteger <* space
  string "happiness units by sitting next to" <* space
  c <- many (anySingleBut '.') <* char '.' <* eof
  return (Map.singleton a (Map.singleton c (b i)))

happyPairMap :: HappyMap -> PairMap
happyPairMap happyMap = let
  names = Map.keys happyMap
  l = [([x, y], (happyMap Map.! x) Map.! y + (happyMap Map.! y) Map.! x) | x <- names, y <- names, x /= y]
  in Map.fromList l

happyPairMap' :: HappyMap -> PairMap
happyPairMap' happyMap = let
  names = Map.keys happyMap
  l = [([x, y], Map.findWithDefault 0 y (Map.findWithDefault Map.empty x happyMap) + Map.findWithDefault 0 x (Map.findWithDefault Map.empty y happyMap)) | x <- "Me" : names, y <- "Me" : names, x /= y]
  in Map.fromList l

cyclePair :: [a] -> [[a]]
cyclePair x = divvy 2 1 (x ++ [head x])

calcHappiness :: PairMap -> [[String]] -> Int
calcHappiness p = sum . map (p Map.!)

day13 :: IO ()
day13 = do
  happyMap <- Map.unionsWith Map.union . map (fromJust . parseMaybe inputParser) . lines <$> readFile "input13.txt"
  let guests = Map.keys happyMap
      guests' = "Me" : Map.keys happyMap
  putStrLn ("day13a: " ++ show (maximum $ map (calcHappiness (happyPairMap happyMap) . cyclePair) $ permutations guests))
  putStrLn ("day13b: " ++ show (maximum $ map (calcHappiness (happyPairMap' happyMap) . cyclePair) $ permutations guests'))
module Day15 (day15) where

import MyLib (Parser, signedInteger)
import Text.Megaparsec (MonadParsec(eof, label), many, parseMaybe, anySingle, anySingleBut)
import Control.Applicative ((<|>))
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (space, char, eol, string)
import Data.Maybe (fromJust)

data Ingredient = Ingredient {name :: String, properties :: [Int]}
  deriving (Show, Eq)

inputParser :: Parser Ingredient
inputParser = do
  name <- many (anySingleBut ':')
  string ": capacity "
  capacity <- signedInteger
  string ", durability "
  durability <- signedInteger
  string ", flavor "
  flavor <- signedInteger
  string ", texture "
  texture <- signedInteger
  string ", calories "
  calories <- signedInteger <* eof
  return $ Ingredient name [capacity, durability, flavor, texture, calories]

sumVariants :: Int -> Int -> [[Int]]
sumVariants n target = f n target [] []
  where
    f :: Int -> Int -> [Int] -> [[Int]] -> [[Int]]
    f n' target' l
      | n' == 0 && target' == 0 = (l :)
      | n' == 1 = ((target' : l) :)
      | n' > 1 && target' == 0 = ((replicate n' 0 ++ l) :)
      | n' > 1 && target' > 0 = foldr (.) id [f (n' - 1) (target' - a) (a : l) | a <- [0 .. target']]

-- day15a :: Int -> [Ingredient] -> [[[Int]]]
day15a total i = let
  n = length i
  sumV = sumVariants n total
  l = map (product . init . map (max 0) . foldr (zipWith (+)) (repeat 0) . zipWith scoreIngredient i) sumV
  in l

day15b total i = let
  n = length i
  sumV = sumVariants n total
  l = map (product . init) . filter ((== 500) . last) $ map (map (max 0) . foldr (zipWith (+)) (repeat 0) . zipWith scoreIngredient i) sumV
  in l

scoreIngredient :: Ingredient -> Int -> [Int]
scoreIngredient x y = map (y *) x.properties

day15 :: IO ()
day15 = do
  ingredients <- map (fromJust . parseMaybe inputParser) . lines <$> readFile "input15.txt"
  -- print ingredients
  -- print $ sumVariants (length ingredients) 100
  putStrLn ("day15a: " ++ show (maximum $ day15a 100 ingredients))
  putStrLn ("day15b: " ++ show (maximum $ day15b 100 ingredients))
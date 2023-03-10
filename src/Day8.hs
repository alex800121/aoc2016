module Day8 where

import MyLib
import Text.Megaparsec (parseMaybe, parseTest, many, anySingle)
import Text.Megaparsec.Char (space, hexDigitChar, string)
import Control.Applicative ((<|>))
import Data.Char
import Data.Maybe (fromJust)

stringParser :: Parser String
stringParser = many (charParser <|> anySingle)

charParser :: Parser Char
charParser = 
      (string "\\x" >> hexDigitChar >>= \x -> hexDigitChar >>= \y -> return (chr (digitToInt x * 16 + digitToInt y)))
  <|> (string "\\\\" >> return '\\')
  <|> (string "\\\"" >> return '"')

day8 :: IO ()
day8 = do
  ins <- lines <$> readFile "input8.txt"
  let parsed = map (fromJust . parseMaybe stringParser . init . tail) ins
      encoded = map show ins
  putStrLn ("day8a: " ++ show (sum $ zipWith (-) (map length ins) (map length parsed)))
  putStrLn ("day8b: " ++ show (sum $ zipWith (-) (map length encoded) (map length ins)))
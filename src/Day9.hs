module Day9 where

import Paths_AOC2016
import Control.Monad ((>=>))
import Data.Char (isUpper)
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Fix (fix)

data NString = Sin Char | Mul Int [NString]
  deriving (Show, Eq, Ord)

sinParser :: Parser NString
sinParser = Sin <$> satisfy isUpper

nstringParser :: Parser NString -> Parser NString
nstringParser f = mulParser f <|> sinParser

mulParser :: Parser NString -> Parser NString
mulParser f = do
  char '('
  n <- signedInteger
  char 'x'
  m <- signedInteger
  char ')'
  c <- count n anySingle
  rest <- getInput
  d <- setInput c >> many f
  setInput rest
  pure $ Mul m d

calcLength :: NString -> Int
calcLength (Sin _) = 1
calcLength (Mul n s) = n * sum (map calcLength s)

day9 :: IO ()
day9 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  let a = parseMaybe (many (nstringParser (Sin <$> anySingle))) input
      b = parseMaybe (many (nstringParser (fix nstringParser))) input
  print $ fmap (sum . map calcLength) a
  print $ fmap (sum . map calcLength) b

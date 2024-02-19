module Day7 where

import Data.Either
import Data.List.Split (divvy)
import Data.Maybe (mapMaybe)
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char

type IP = ([String], [String])

ipParser :: Parser IP
ipParser =
  partitionEithers
    <$> many
      ( (Left <$> between (char '[') (char ']') (many (satisfy (`notElem` "[]"))))
          <|> (Right <$> some (satisfy (`notElem` "[]")))
      )

hasABBA :: String -> Bool
hasABBA = any f . divvy 4 1
  where
    f [a, b, c, d] = a == d && b == c && a /= b

hasABA :: IP -> Bool
hasABA (x, y) = any (`elem` as) bs
  where
    as = map g $ concatMap (filter f . divvy 3 1) y
    f [a, b, c] = a == c && a /= b
    g [a, b, _] = [b, a, b]
    bs = concatMap (divvy 3 1) x

day7 :: IO ()
day7 = do
  input <- mapMaybe (parseMaybe ipParser) . lines <$> readFile "input/input7.txt"
  print
    . length
    $ filter ((&&) <$> (not . any hasABBA . fst) <*> (any hasABBA . snd)) input
  print
    . length
    $ filter hasABA input

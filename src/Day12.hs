module Day12 (day12) where

import MyLib
import Text.Megaparsec (parseMaybe, parseTest, anySingle, anySingleBut, eof, many)
import Text.Megaparsec.Char (char, string, space)
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

data Content = O Object | S String | I Int | A Array deriving (Show, Eq, Ord)
data Property a = Property {name :: String, content :: a} deriving (Show, Eq, Ord)
type Array = [Content] 
type Object = [Property Content]

instance Functor Property where
  fmap f (Property name content) = Property name (f content)

integerParser :: Parser [Int]
integerParser = 
      (eof >> return [])
  <|> (signedInteger >>= \x -> (x :) <$> integerParser)
  <|> (anySingle >> integerParser)

stringParser :: Parser String
stringParser = char '"' >> many (anySingleBut '"') <* char '"'

intParser :: Parser Int
intParser = signedInteger

contentParser :: Parser Content
contentParser = 
      (O <$> objectParser)
  <|> (A <$> arrayParser)
  <|> (I <$> intParser)
  <|> (S <$> stringParser)

objectParser :: Parser Object
objectParser = (char '}' >> return []) <|> do
  char '{' <|> char ','
  (char '}' >> return []) <|> do
    name <- stringParser
    char ':'
    content <- contentParser
    (Property name content :) <$> objectParser

arrayParser :: Parser Array
arrayParser = (char ']' >> return []) <|> do
  char '[' <|> char ','
  (char ']' >> return []) <|> (contentParser >>= \x -> (x :) <$> arrayParser)

day12 :: IO ()
day12 = do
  input <- readFile "input12.txt"
  let day12a = sum . fromJust . parseMaybe integerParser $ input
      day12b = fromJust $ parseMaybe objectParser input
  putStrLn ("day12a: " ++ show day12a)
  putStrLn ("day12b: " ++ show (contentSum $ fromJust $ cleanContent (O day12b)))

contentHasRed :: Content -> Bool
contentHasRed (S "red") = True
contentHasRed _ = False

propertyHasRed :: Property Content -> Bool
propertyHasRed (Property name content) = contentHasRed content

objectHasRed :: Object -> Bool
objectHasRed = any propertyHasRed

cleanContent :: Content -> Maybe Content
cleanContent (O o) = if objectHasRed o then Nothing else Just (O (
  map (fmap fromJust) $ filter ((/= Nothing) . (.content)) $ map (fmap cleanContent) o
  ))
cleanContent (S s) = if s == "red" then Nothing else Just (S s)
cleanContent (A a) = Just (A (map fromJust $ filter (/=Nothing) $ map cleanContent a))
cleanContent (I i) = Just (I i)

contentSum :: Content -> Int
contentSum (I i) = i
contentSum (S s) = 0
contentSum (O o) = sum $ map (contentSum . (.content)) o
contentSum (A a) = sum $ map contentSum a
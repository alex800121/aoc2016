{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Day19 (day19) where

import MyLib hiding (Nat (..))
import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec ( parseMaybe, anySingle, anySingleBut, many, eof, Parsec, token, satisfy, single, noneOf, try, parseTest )
import Text.Megaparsec.Char ( char, eol, space, string )
import Control.Applicative ((<|>))
import Control.Monad.Combinators (choice)
import Data.Char (isUpper)
import Data.Maybe (catMaybes, fromJust)
import Data.Void (Void)

newtype S t a = S {fromS :: t a} deriving (Show, Eq)
type LString = S [] Char
type LLString = S [] String

instance Ord (S [] Char) where
  compare (S a) (S b)= (compare `on` countX "Y") a b <> (compare `on` countX "Rn") a b <> compare a b

instance Ord (S [] String) where
  compare (S a) (S b) =
    (compare `on` length) a b <> 
    compare a b

countX :: Eq a => [a] -> [a] -> Int
countX x = length . filter (== x) . split (onSublist x)

instance (Functor t) => Functor (S t) where
  fmap f (S x) = S (fmap f x)

ruleParser :: String -> Map String [String]
ruleParser s = let
  x : _ : y : _ = words s
  in Map.singleton x [y]

replaceString :: Map String [String] -> String -> [String]
replaceString m s = concat l
  where
    k = Map.keys m
    s' = zip (inits s) (tails s)
    l = do
      (i, t) <- s'
      k' <- k
      return $ map (i ++) (replaceHead m k' t)

replaceHead :: Map String [String] -> String -> String -> [String]
replaceHead m x y = if x `isPrefixOf` y then map (++ drop (length x) y) (m Map.! x) else []

replaceHead3 :: Map [String] String -> [String] -> [String] -> Maybe [String]
replaceHead3 m x y = if x `isPrefixOf` y then Just $ (m Map.! x) : drop (length x) y else Nothing

replaceString3 :: Map [String] String -> [String] -> [[String]]
replaceString3 m s = catMaybes l
  where
    k = Map.keys m
    s' = zip (inits s) (tails s)
    l = do
      (i, t) <- s'
      k' <- k
      return $ fmap (i ++) (replaceHead3 m k' t)

step :: Map [String] String -> Map LLString Int -> Map LLString Int
step m xs = trace (show $ fromS x) xs'
  where
    (x, i) = Map.findMin xs
    xs' = Map.delete x $ foldl' (\acc y -> Map.insertWith min (S y) (i + 1) acc) xs (replaceString3 m (fromS x))

run :: LLString -> Map [String] String -> Map LLString Int -> Int
run target m x = case x Map.!? target of
  Nothing -> run target m (step m x)
  Just i -> i

ruleParser3 :: String -> Map E E
ruleParser3 s = let
  x : _ : y : _ = words s
  l = tail $ split (keepDelimsL $ whenElt isUpper) y
  l' = case length l of
    8 -> let a : _ : c : _ : e : _ : g : _ = l in XRnXYXYXAr (I a) (I c) (I e) (I g)
    6 -> let a : _ : c : _ : e : _ = l in XRnXYXAr (I a) (I c) (I e)
    4 -> let a : _ : c : _  = l in XRnXAr (I a) (I c)
    2 -> let a : b : _ = l in E [I a, I b]
  in Map.singleton l' (I x)

data E = I String | E [E] | XRnXAr E E | XRnXYXAr E E E | XRnXYXYXAr E E E E deriving (Show, Eq, Ord)

type ParserE = Parsec Void [String]

eToInt :: E -> Int
eToInt (I _) = 0
eToInt (E l) = length l - 1 + sum (map eToInt l)
eToInt (XRnXAr a b) = eToInt a + eToInt b + 1
eToInt (XRnXYXAr a b c) = eToInt a + eToInt b + eToInt c + 1
eToInt (XRnXYXYXAr a b c d) = eToInt a + eToInt b + eToInt c + eToInt d + 1

parserE :: ParserE E
parserE = E <$> many (choice [try parser2Y, try parser1Y, try parser0Y, try parserI])

parserI :: ParserE E
parserI = I <$> noneOf ["Rn", "Y", "Ar"]

parser0Y :: ParserE E
parser0Y = do
  a <- E <$> many parserI
  single "Rn"
  b <- parserE
  single "Ar"
  return (XRnXAr a b)

parser1Y :: ParserE E
parser1Y = do
  a <- E <$> many parserI
  single "Rn"
  b <- parserE
  single "Y"
  c <- parserE
  single "Ar"
  return (XRnXYXAr a b c)

parser2Y :: ParserE E
parser2Y = do
  a <- E <$> many parserI
  single "Rn"
  b <- parserE
  single "Y"
  c <- parserE
  single "Y"
  d <- parserE
  single "Ar"
  return (XRnXYXYXAr a b c d)


day19 :: IO ()
day19 = do
  x : y : _ <- splitOn "\n\n" <$> readFile "input19.txt"
  let
    rules = Map.unionsWith (<>) $ map ruleParser $ lines x
    rules3 = Map.unions $ map ruleParser3 $ lines x
    y3 = parseMaybe parserE $ tail $ split (keepDelimsL $ whenElt isUpper) y
  putStrLn ("day19a: " ++ show (length $ nub $ replaceString rules y))
  putStrLn ("day19b: " ++ show y3)

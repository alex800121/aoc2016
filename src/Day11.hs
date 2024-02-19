module Day11 where

import Control.Monad (guard)
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlpha)
import Data.List (partition)
import Data.PQueue.Prio.Min (MinPQueue (..))
import qualified Data.PQueue.Prio.Min as PQ
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import MyLib
import Text.Megaparsec
import Text.Megaparsec.Char

data Item
  = Chip {_getChar :: !Char}
  | Generator {_getChar :: !Char}
  deriving (Show, Eq, Ord)

type Floor = Set Item

type Facility = Array Int Floor

type GameState = (Int, Facility)

type Pair = (Int, Int)

type Facility' = [Pair]

type GameState' = (Int, Facility')

convert :: Facility -> Facility'
convert x =
  [ (c', g')
    | (a, c') <- c,
      (b, g') <- g,
      a == b
  ]
  where
    allItems =
      [ (a, b)
        | (b, t) <- A.assocs x,
          a <- Set.toList t
      ]
    (c, g) =
      bimap (map (first _getChar)) (map (first _getChar)) $
        partition (\(x, _) -> case x of Chip _ -> True; _ -> False) allItems

validFac' :: Facility' -> Bool
validFac' cg =
  null
    [ c
      | (c, g) <- cg,
        c /= g,
        c `elem` gs
    ]
  where
    (cs, gs) = unzip cg

partitionItems :: Floor -> (Set Char, Set Char)
partitionItems =
  bimap (Set.map _getChar) (Set.map _getChar)
    . Set.partition (\case Chip _ -> True; _ -> False)
{-# INLINE partitionItems #-}

-- The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
-- The fourth floor contains nothing relevant.
nthParser :: Parser Int
nthParser =
  (string "first" >> pure 1)
    <|> (string "second" >> pure 2)
    <|> (string "third" >> pure 3)
    <|> (string "fourth" >> pure 4)

itemParser :: Parser Item
itemParser = do
  char 'a' >> space
  name <- many (satisfy isAlpha)
  optional $ string "-compatible"
  space
  t <- (string "microchip" >> pure Chip) <|> (string "generator" >> pure Generator)
  pure $ t (head name)

floorParser :: Parser (Int, Floor)
floorParser = do
  n <- string "The " >> nthParser <* string " floor contains "
  s <-
    (string "nothing relevant" >> pure Set.empty)
      <|> Set.fromList
      <$> (itemParser `sepBy` (string ", " >> optional (string "and ")))
  char '.'
  pure (n, s)

facilityParser :: Parser Facility
facilityParser = do
  floors <- floorParser `sepEndBy` newline
  let fn = map fst floors
      b = (minimum fn, maximum fn)
  pure $ A.array b floors

type Range = (Int, Int)

validFloor :: Floor -> Bool
validFloor = uncurry go . partitionItems
  where
    go c g = Set.null c' || Set.null g
      where
        c' = c Set.\\ g
        g' = g Set.\\ c

nextStep :: Range -> GameState -> [GameState]
nextStep b (n, fac) = do
  nextN <- [n - 1, n + 1]
  guard $ A.inRange b nextN
  let currentFloor = fac A.! n
      nextFloor = fac A.! nextN
  itemN <- [1, 2]
  (picked, rest) <- pickNSplit itemN $ Set.toList currentFloor
  let currentFloor' = Set.fromList rest
      nextFloor' = nextFloor <> Set.fromList picked
  -- traceShowM currentFloor'
  -- traceShowM nextFloor'
  guard $ validFloor currentFloor'
  guard $ validFloor nextFloor'
  pure (nextN, fac A.// [(n, currentFloor'), (nextN, nextFloor')])

calcHeu :: Range -> GameState -> Int
calcHeu b (_, fac) = sum $ map (\(x, y) -> (snd b - x) * length y) $ A.assocs fac

aStar :: Range -> (GameState -> Int) -> Set GameState -> MinPQueue Int (GameState, Int) -> Maybe (GameState, Int)
aStar _ _ _ Empty = Nothing
aStar b f visited ((heuN, (g, n)) :< qs)
  -- \| traceShow (heuN, g, n) False = undefined
  | calcHeu b g == 0 = Just (g, n)
  | otherwise = aStar b f visited' (PQ.union qs qs')
  where
    nextG = filter (`Set.notMember` visited) $ nextStep b g
    visited' = Set.union visited $ Set.fromList nextG
    qs' =
      PQ.fromList
        [ (f g' + n, (g', n + 1))
          | g' <- nextG
        ]

day11 :: IO ()
day11 = do
  Just input <- parseMaybe facilityParser <$> readFile "input/input11.txt"
  Just input <- parseMaybe facilityParser <$> readFile "input/test11.txt"
  let b = A.bounds input
      initState = (1, input)
      input' = input A.// [(1, Set.union (Set.fromList [f c | f <- [Chip, Generator], c <- "ed"]) (input A.! 1))]
      b' = A.bounds input'
      initState' = (1, input')
  -- print (snd <$> aStar b (calcHeu b) Set.empty (PQ.singleton (calcHeu b initState) (initState, 0)))
  -- print (snd <$> aStar b' (calcHeu b') Set.empty (PQ.singleton (calcHeu b' initState') (initState', 0)))
  print $ convert input
  print $ validFac' $ convert input

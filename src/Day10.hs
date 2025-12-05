module Day10 where

import Paths_AOC2016
import Control.Monad.Trans.State (state)
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (find, sort)
import Data.Maybe (isJust)
import Control.Monad (join)

type Bots = IntMap Bot

data Bot = Bot
  { _chips :: ![Int],
    _toLow :: !(Either Int Int),
    _toHigh :: !(Either Int Int)
  }
  deriving (Show, Ord, Eq)

fuseBot :: Bot -> Bot -> Bot
fuseBot (Bot c l h) (Bot c' l' h') = Bot (c <> c') (max l l') (max h h')

-- bot 119 gives low to bot 18 and high to bot 3
-- value 53 goes to bot 102
readBot :: String -> IntMap Bot
readBot s = case words s of
  [_, n, _, _, _, low, lown, _, _, _, high, highn] -> IM.singleton (read n) $ Bot [] (f low $ read lown) (f high $ read highn)
  [_, x, _, _, _, n] -> IM.singleton (read n) $ Bot [read x] (Left minBound) (Left minBound)
  where
    f "bot" = Right
    f "output" = Left

step :: (IntMap [Int], Bots) -> (IntMap [Int], Bots)
step (a, b) = IM.foldlWithKey' f (a, b) b
  where
    f acc k bot
      | length c == 2 = second (IM.adjust (\x -> x {_chips = []}) k) $ g _toHigh maxC $ g _toLow minC acc
      | otherwise = acc
      where
        c = _chips bot
        minC = minimum c
        maxC = maximum c
        g toWhere y = case toWhere bot of
          Left n -> first (IM.insertWith (<>) n [y])
          Right n -> second (IM.adjust (\x -> x {_chips = y : _chips x}) n)

day10 :: IO ()
day10 = do
  -- input <- IM.unionsWith fuseBot . map readBot . lines <$> readFile "input/test10.txt"
  input <- IM.unionsWith fuseBot . map readBot . lines <$> (getDataDir >>= readFile . (++ "/input/input10.txt"))
  let a = iterate step (IM.empty, input)
  print $ fmap (head . IM.keys) $ find (not . IM.null) $ map (IM.filter ((== [17, 61]) . sort . _chips) . snd) a
  print $ fmap (product . map head) $ join $ find isJust $ map (\(o, _) -> traverse (o IM.!?) [0, 1, 2]) a

module Day21 (day21) where

import MyLib
import Data.List.Split
import Data.List
import Debug.Trace

type Item = (Int, Int, Int)
data Player = P { hit :: Int, damage :: Int, armor :: Int }
  deriving (Show, Eq, Ord)
type Boss = Player

boss :: Boss
boss = P 100 8 2

-- buy :: [Item] -> [Item] -> [Item] -> [((Int, Player), (Item, Item, Item))]
buy :: [Item] -> [Item] -> [Item] -> [(Int, Player)]
buy w a r = do
  w1 <- w
  a1 <- (0, 0, 0) : a
  let f (d1, d2, d3) (e1, e2, e3) = (d1 + e1, d2 + e2, d3 + e3)
  r1 <- map (foldr f (0, 0, 0)) $ filter ((<= 2) . length) $ subsequences r
  let (x, y, z) = foldr f (0, 0, 0) [w1, a1, r1]
  -- pure ((x, P 100 y z), (w1, a1, r1))
  pure (x, P 100 y z)

weapons :: [Item]
weapons =
  [ (8, 4, 0)
  , (10, 5, 0)
  , (25, 6, 0)
  , (40, 7, 0)
  , (74, 8, 0)
  ]

armors :: [Item]
armors =
  [ (13, 0, 1)
  , (31, 0, 2)
  , (53, 0, 3)
  , (75, 0, 4)
  , (102, 0, 5)
  ]

rings :: [Item]
rings =
  [ (25, 1, 0)
  , (50, 2, 0)
  , (100, 3, 0)
  , (20, 0, 1)
  , (40, 0, 2)
  , (80, 0, 3)
  ]

attack :: Player -> Player -> Player
attack attack@(P h1 d1 a1) defend@(P h2 d2 a2) = P (h2 - attackPoint) d2 a2
  where
    attackPoint = max 1 (d1 - a2)

playGame :: Boss -> Player -> Bool
playGame boss player
  -- | trace (show (boss, player)) False = undefined
  | boss'.hit <= 0 = True
  | player'.hit <= 0 = False
  | otherwise = playGame boss' player'
  where
    boss' = player `attack` boss
    player' = boss `attack` player
  
day21 :: IO ()
day21 = do
  -- mapM_ print $ sort $ buy weapons armors rings
  putStrLn $ ("day21a: " ++) $ show $ fst $ head $ dropWhile (not . playGame boss . snd) $ sort $ buy weapons armors rings
  putStrLn $ ("day21b: " ++) $ show $ fst $ head $ dropWhile (playGame boss . snd) $ reverse $ sort $ buy weapons armors rings
  -- putStrLn $ ("day21b: " ++) $ show $ filter (not . snd) $ map (fmap (playGame boss)) $ reverse $ sort $ buy weapons armors rings

{-# LANGUAGE LambdaCase #-}
module Day22 (day22) where

import MyLib
import GHC.TypeLits (Symbol)
import Control.Monad (guard)
import Data.List (foldl', insert)
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set

data GameState = G { spentMana :: Int,  turn :: Turn, boss :: Boss, player :: Player, spells :: [Spell] } deriving (Show, Eq, Ord)
data Turn = BossT | PlayerT deriving (Show, Eq, Ord)
data Boss = B { bossHp :: Int, bossDamage :: Int } deriving (Show, Eq, Ord)
data Player = P { playerHp :: Int, playerMana :: Int, playerArmor :: Int } deriving (Show, Eq, Ord)
data Spell = S { spellType :: SpellType, counter :: Int } deriving (Show, Eq, Ord)
data SpellType = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq, Ord)


testPlayer = P 10 250 0 :: Player
testBoss1 = B 13 8 :: Boss
testBoss2 = B 14 8 :: Boss
initPlayer = P 50 500 0 :: Player
initBoss = B 51 9 :: Boss
initSpells =
  [ S MagicMissile 0
  , S Drain 0
  , S Shield 0
  , S Poison 0
  , S Recharge 0
  ] :: [Spell]
initGameState = G 0 PlayerT initBoss initPlayer initSpells :: GameState
testGameState1 = G 0 PlayerT testBoss1 testPlayer initSpells :: GameState
testGameState2 = G 0 PlayerT testBoss2 testPlayer initSpells :: GameState

step :: Int -> (Int -> GameState -> [GameState]) -> Set GameState -> Int
step i p g = case choice i p x of
-- step i p g = trace (show x) $ case choice i p x of
  Left n -> n
  Right ys -> step i p $ foldl' (flip Set.insert) xs ys
  where
    (x, xs) = Set.deleteFindMin g

choice :: Int -> (Int -> GameState -> [GameState]) -> GameState -> Either Int [GameState]
choice i p g
  | g'.boss.bossHp <= 0 = Left (g.spentMana)
  | g.player.playerHp <= 0 = Right []
  | g'.turn == BossT = Right [bossTurn g']
  | g'.turn == PlayerT = Right $ p i g'
  where
    g' = effects g

bossTurn :: GameState -> GameState
bossTurn g = g { turn = PlayerT, player = g.player { playerHp = g.player.playerHp - max 1 (g.boss.bossDamage - g.player.playerArmor) } }


effects :: GameState -> GameState
effects g = g' { spells = map (\(S a n) -> S a (max 0 (n - 1))) g.spells }
  where
    g' = foldr interpretEffect g g.spells

interpretEffect :: Spell -> GameState -> GameState
interpretEffect (S MagicMissile _) g = g
interpretEffect (S Drain _) g = g
interpretEffect (S Shield n) g 
  | n > 0 = g { player = g.player { playerArmor = 7 } }
  | otherwise = g { player = g.player { playerArmor = 0 } }
interpretEffect (S Poison n) g
  | n > 0 = g { boss = g.boss { bossHp = g.boss.bossHp - 3 } }
  | otherwise = g
interpretEffect (S Recharge n) g
  | n > 0 = g { player = g.player { playerMana = g.player.playerMana + 101 } }
  | otherwise = g

playerTurn :: Int -> GameState -> [GameState]
playerTurn i f = do
  let g = f { player = f.player { playerHp = f.player.playerHp - i } }
  guard $ g.player.playerHp > 0
  S st n <- g.spells
  guard $ n == 0
  let
    g' = case st of
      MagicMissile ->
        g { spentMana = g.spentMana + 53
          , boss = g.boss { bossHp = g.boss.bossHp - 4 }
          , player = g.player { playerMana = g.player.playerMana - 53 }
          }
      Drain ->
        g { spentMana = g.spentMana + 73
          , boss = g.boss { bossHp = g.boss.bossHp - 2 }
          , player = g.player { playerMana = g.player.playerMana - 73, playerHp = g.player.playerHp + 2 }
          }
      Shield ->
        g { spentMana = g.spentMana + 113
          , player = g.player { playerMana = g.player.playerMana - 113 }
          , spells = map (\case ; S Shield _ -> S Shield 6 ; a -> a) g.spells
          }
      Poison ->
        g { spentMana = g.spentMana + 173
          , player = g.player { playerMana = g.player.playerMana - 173 }
          , spells = map (\case ; S Poison _ -> S Poison 6 ; a -> a) g.spells
          }
      Recharge ->
        g { spentMana = g.spentMana + 229
          , player = g.player { playerMana = g.player.playerMana - 229 }
          , spells = map (\case ; S Recharge _ -> S Recharge 5 ; a -> a) g.spells
          }
  guard $ g'.player.playerMana > 0
  return $ g' { turn = BossT }

day22 :: IO ()
day22 = do
  putStrLn $ ("day22a: " ++) $ show $ step 0 playerTurn $ Set.singleton initGameState
  putStrLn $ ("day22b: " ++) $ show $ step 1 playerTurn $ Set.singleton initGameState

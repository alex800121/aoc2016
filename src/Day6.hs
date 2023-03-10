module Day6 (day6) where

import Data.Array (Array)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (splitOneOf)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Effect.State
import Control.Carrier.State.Strict
import Control.Monad (forM_)

type Index = (Int, Int)

data Op = Turn Light | Toggle deriving (Show, Eq)
data LightInstruction = LI {ins :: Op, start :: Index, end :: Index} deriving (Show, Eq)
data Light = On | Off deriving (Show, Eq)

instance Enum Light where
  fromEnum On = 0
  fromEnum Off = 1
  toEnum x = case x `mod` 2 of
    0 -> On
    1 -> Off

initLights :: Set Index
initLights = Set.empty

initLightsb :: Map Index Int
initLightsb = Map.empty

inputParser :: String -> LightInstruction
inputParser s = let
  x : xs = splitOneOf " ," s
  ins' : x1 : y1 : _ : x2 : y2 : _ = if x == "toggle" then x : xs else xs
  ins = case ins' of 
    "toggle" -> Toggle
    "on" -> Turn On
    "off" -> Turn Off
    _ -> undefined
  in LI ins (read x1, read y1) (read x2, read y2)

runIns :: LightInstruction -> Set Index -> Set Index
runIns (LI ins (x1, y1) (x2, y2)) s = case ins of
  Toggle -> Set.union s s' Set.\\ s''
  Turn On -> Set.union s s'
  Turn Off -> s Set.\\ s'
  where
    s' = Set.fromList [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
    s'' = Set.intersection s s'

runIns' :: [LightInstruction] -> Set Index -> Set Index
runIns' ins s = run . execState s $ forM_ ins (modify . runIns)

runInsb :: LightInstruction -> Map Index Int -> Map Index Int
runInsb (LI ins (x1, y1) (x2, y2)) s = case ins of
  Toggle -> foldr (\k -> Map.insertWith (+) k 2) s s'
  Turn On -> foldr (\k -> Map.insertWith (+) k 1) s s'
  Turn Off -> foldr (Map.update (\x -> if x <= 0 then Nothing else Just (x - 1))) s s'
  where
    s' = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

runInsb' :: [LightInstruction] -> Map Index Int -> Map Index Int
runInsb' ins s = run . execState s $ forM_ ins (modify . runInsb)

day6 :: IO ()
day6 = do
  input <- map inputParser . lines <$> readFile "input6.txt"
  putStrLn ("day6a: " ++ show (Set.size $ runIns' input initLights))
  putStrLn ("day6b: " ++ show (sum $ Map.elems $ runInsb' input initLightsb))
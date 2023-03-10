module Day10 (day10) where

import MyLib
import Data.List
import Control.Effect.State
import Control.Carrier.State.Strict
import Control.Monad (forM_)

input :: String
input = "1113122113"

step = concatMap (\x -> show (length x) ++ [head x]) . group

step' :: Int -> String -> String
step' n i = run . execState i $ forM_ [1..n] $ \_ -> modify step

day10 :: IO ()
day10 = do
  putStrLn ("day10a: " ++ show (length $ iterate step input !! 40))
  putStrLn ("day10b: " ++ show (length $ iterate step input !! 50))
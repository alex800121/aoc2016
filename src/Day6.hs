module Day6 where
import Paths_AOC2016
import Data.List
import Data.Function (on)

day6 :: IO ()
day6 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  putStrLn . map (head . maximumBy (compare `on` length) . group . sort) $ transpose input
  putStrLn . map (head . minimumBy (compare `on` length) . group . sort) $ transpose input

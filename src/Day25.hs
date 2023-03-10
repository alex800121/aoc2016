
module Day25 (day25) where

import MyLib

initCode = 20151125 :: Int

step :: Int -> Int
step = (`mod` 33554393) . (* 252533)

stepAt :: Int -> Int -> Int
stepAt i n
  | n <= 0 = i
  | otherwise = stepAt (step i) (n - 1)

codeAtN :: Int -> Int -> Int
codeAtN row column = y + column
  where
    x = row + column - 1
    y = (x * (x - 1)) `div` 2

day25 :: IO ()
day25 = do
  putStrLn $ ("day25a: " ++) $ show $ stepAt initCode (codeAtN 2978 3083 - 1)
  putStrLn $ ("day25b: " ++) $ show "Done!"

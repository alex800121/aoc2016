module Day16 where
import Data.List (find)
import Debug.Trace

initInput = "10001001100000001"

l = 272
l' = 35651584

convertInput = map (== '1')
step :: [Bool] -> [Bool]
step = (<>) <$> id <*> (False :) . reverse . map not

checkSum :: [Bool] -> [Bool]
checkSum xs = f id xs
  where
    f acc [] = checkSum (acc [])
    f acc [x] = xs
    f acc (x : y : ys) = f (acc . ((x == y) :)) ys

day16 :: IO ()
day16 = do
  -- input <- readFile "input/input16.txt"
  -- print initInput
  print
    . fmap (map (\x -> if x then '1' else '0') . checkSum . take l)
    . find ((>= l) . length)
    . iterate step 
    $ convertInput initInput
    -- . fmap (map (\x -> if x then '1' else '0') . checkSum . take 20)
    -- . find ((>= 20) . length)
    -- $ iterate step $ convertInput "10000"
  print
    . fmap (map (\x -> if x then '1' else '0') . checkSum . take l')
    . find ((>= l') . length)
    . iterate step 
    $ convertInput initInput
    

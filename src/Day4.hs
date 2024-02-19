module Day4 where

import Data.Char (chr, ord)
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)

type S = ([String], Int)

readString :: String -> (S, String)
readString s = ((b, read c), e)
  where
    a = splitOn "-" s
    b = init a
    [c, d] = splitOn "[" $ last a
    e = init d

checkSum :: S -> String -> Bool
checkSum (s, _) c = c == take 5 (map head $ sortBy (\x y -> (compare `on` length) y x <> (compare `on` head) x y) s')
  where
    s' = group $ sort $ concat s

decryptChar :: Int -> Char -> Char
decryptChar i = chr . (+ ord 'a') . (`mod` 26) . (+ i) . subtract (ord 'a') . ord

decrypt :: S -> String
decrypt (s, i) = unwords $ map (map (decryptChar i)) s

day4 :: IO ()
day4 = do
  input <- map readString . lines <$> readFile "input/input4.txt"
  print $ sum $ map (snd . fst) $ filter (uncurry checkSum) input
  print $ fmap fst $ find (("north" `isInfixOf`) . snd) $ map (((,) <$> snd <*> decrypt) . fst) input

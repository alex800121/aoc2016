{-# LANGUAGE OverloadedStrings #-}
module Day5 (day5) where

import MyLib
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)
import Data.List (isPrefixOf, delete)
import Data.Char (digitToInt)
import Debug.Trace

input :: ByteString
input = "cxdnnyjw"
-- input = "abc"

inputList :: [ByteString]
inputList = map ((input <>) . fromString . show)  [0..]

hashList :: [String]
hashList = map (show . md5) inputList

has5Zero :: [String]
has5Zero = filter ("00000" `isPrefixOf`) hashList

take8NonZero :: String
take8NonZero = map (!! 5) $ take 8 has5Zero

buildPW :: String -> String -> [String] -> String
buildPW temp seen (x : xs)
  | trace (temp ++ ',' : seen) False = undefined
  | null seen =  temp
  | pos `notElem` seen = buildPW temp seen xs
  | otherwise = buildPW temp' seen' xs
  where
    pos = x !! 5
    pos' = digitToInt pos
    seen' = delete pos seen
    temp' = take pos' temp ++ (x !! 6) : drop (pos' + 1) temp

day5 :: IO ()
day5 = do
  -- putStrLn $ "day5a: " ++ take8NonZero
  putStrLn $ "day5b: " ++ buildPW "xxxxxxxx" "01234567" has5Zero

{-# LANGUAGE OverloadedStrings #-}

module Day4 (day4) where

import Data.List
import Data.Hash.MD5

input :: String
input = "bgvyzdsv"

prefix :: String
prefix = "00000"

prefix' :: String
prefix' = "000000"

day4a :: String -> Int
day4a s = head $ dropWhile (not . (prefix `isPrefixOf`) . md5s . Str . (s ++) . show) [0..] 

day4b :: String -> Int
day4b s = head $ dropWhile (not . (prefix' `isPrefixOf`) . md5s . Str . (s ++) . show) [0..] 

day4 :: IO ()
day4 = do
  putStrLn ("day4a: " ++ show (day4a input))
  putStrLn ("day4b: " ++ show (day4b input))
{-# LANGUAGE PackageImports #-}

module Day5 where

import Paths_AOC2016
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.String (IsString (..))
import Data.Word (Word8)
import "cryptohash-md5" Crypto.Hash.MD5
import Numeric (showHex)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.List (delete)

input = fromString "ojvtpuvg"

postFixHash :: ByteString -> Int -> ByteString
postFixHash s i = hash $ s <> fromString (show i)

buildPassword :: ByteString -> Int -> [Word8]
buildPassword b n = take n bs'
  where
    bs = map (B.unpack . postFixHash b) [0 ..]
    bs' = map ((`mod` 16) . (!! 2)) $ filter (\x -> all (== 0) (take 2 x) && (x !! 2) < 16) bs

buildPassword' :: ByteString -> Int -> [Word8]
buildPassword' b n = V.toList $ f bs' ns v
  where
    v = V.fromList (replicate n 0)
    bs = map (B.unpack . postFixHash b) [0 ..]
    bs' = map ((,) <$> fromIntegral . (`mod` 16) . (!! 2) <*> (`div` 16) . (!! 3)) $ filter (\x -> all (== 0) (take 2 x) && (x !! 2) < 16) bs
    ns = [0..(n - 1)]
    f _ [] acc = acc
    f ((x, y) : xs) ns acc
      | x `elem` ns = f xs (delete x ns) acc'
      | otherwise = f xs ns acc
      where
        acc' = acc V.// [(x, y)]

day5 :: IO ()
day5 = do
  -- print $ B.unpack $ postFixHash input 0
  putStrLn $ concatMap (`showHex` "") $ buildPassword input 8
  putStrLn $ concatMap (`showHex` "") $ buildPassword' input 8

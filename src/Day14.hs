{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Day14 where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base16
import Data.ByteString.Internal (w2c)
import Data.Char (digitToInt, intToDigit)
import Data.List (find, findIndices, unfoldr)
import Data.Maybe (fromJust, isJust)
import Data.String (IsString (..))
import qualified "cryptohash-md5" Crypto.Hash.MD5 as MD5

-- input = "abc"
input = "qzyelonm"

repeatedChar n = map (fromString @ByteString . replicate n . intToDigit) [0 .. 15]

fives = repeatedChar 5

encodeByteString :: ByteString -> (Maybe Int, [Int])
encodeByteString b = (digitToInt . w2c . B.head <$> find ((>= 3) . B.length) (B.group b), findIndices (`B.isInfixOf` b) fives)

withPostfix :: Int -> Int -> ByteString -> ByteString
withPostfix i n input = (!! i) $ iterate (encode . MD5.hash) $ input <> fromString (show n)

makeIndices :: [[Int]] -> [[Int]]
makeIndices xs = map (\x -> findIndices (elem x) xs) l
  where
    l = [0 .. 15]

-- calcInput :: (Int -> ByteString -> ByteString) -> ByteString -> Int
calcInput fn input = filter (`g` i5) i3 
  where
    (r3, r5) =
      unzip $
        map (encodeByteString . (`fn` input)) [0 ..]
    f = zip <$> findIndices isJust <*> filter isJust
    h = zip <$> findIndices (not . null) <*> filter (not . null)
    i3 = f r3
    i5 = h r5
    g (x, y) xs = any (any (== fromJust y) . snd) $ takeWhile ((<= x + 1000) . fst) $ dropWhile ((<= x) . fst) xs

day14 :: IO ()
day14 = do
  print $ fst $ (!! 63) $ calcInput (withPostfix 1) input
  -- print $ take 64 $ calcInput (withPostfix 2017) input
  print $ fst $ (!! 63) $ calcInput (withPostfix 2017) input

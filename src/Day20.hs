module Day20 (day20) where

-- import MyLib
import Data.List

inputN :: Int
inputN = 34000000

day20a :: Integral a => a -> a
day20a = (* 10) . sum . factors

day20b :: Integral a => a -> a
day20b n = (* 11) . sum . filter (> (n - 1) `div` 50) . factors $ n

primeFactors :: Integral a => a -> [a]
primeFactors a = f a primes []
  where
    primes = takeWhile (<= sqrtCeiling a) primeSeive
    f 1 _ ls = ls
    f x [] ls = x : ls
    f x (y : ys) ls
      | x `mod` y == 0 = f (x `div` y) (y : ys) (y : ls)
      | otherwise = f x ys ls

primeFactors' :: Integral a => a -> [(Int, a)]
primeFactors' = map ((,) <$> length <*> head) . group . primeFactors

factors :: Integral a => a -> [a]
factors x = let
  p = primeFactors' x
  f [] = [1]
  f ((n, a) : xs) = (*) <$> map (a ^) [0 .. n] <*> f xs
  in f p

sqrtCeiling :: Integral a => a -> a
sqrtCeiling = ceiling . sqrt . fromIntegral

primeSeive :: Integral a => [a]
primeSeive = f [2..]
  where
    f (x : xs) = x : f (filter ((/= 0) . (`mod` x)) xs)

day20 :: IO ()
day20 = do
  putStrLn ("day20a: " ++ show ((+ 1) $ length $ takeWhile (< inputN) $ map day20a [1..]))
  putStrLn ("day20b: " ++ show ((+ 1) $ length $ takeWhile (< inputN) $ map day20b [1..]))
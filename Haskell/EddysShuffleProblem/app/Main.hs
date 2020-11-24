module Main where

import           Data.Int (Int32)

calculateTimes :: Int32 -> Int
calculateTimes n = go 1 (shuffle (part original) [])
    where go n current | original == current = n
                       | otherwise = go (n + 1) (shuffle (part current) [])
          original = [1..n]
          part l = splitAt (length original `div` 2) l
          shuffle ([],[])         result = result
          shuffle ((a:r1),(b:r2)) result = shuffle (r1,r2) (b : a : result)

calculateTimes' :: Int32 -> Int
calculateTimes' n = go 1 2
    where go r 1 = r
          go r m = go (r + 1) ((m * 2) `mod` (n * 2 + 1))

main :: IO ()
main = pure ()

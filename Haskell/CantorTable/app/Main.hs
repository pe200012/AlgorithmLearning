{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Tuple  (swap)
import           Text.Printf (printf)

findingGroupAndPos :: Int -> (Int, Int)
findingGroupAndPos = go 1
    where
        seq = scanl (+) 0 [1..]
        go i num
         | num - (seq !! (i - 1)) == 0 = (i - 1, seq !! (i - 1))
         | num - (seq !! (i - 1)) < [1..] !! i = (i, num - (seq !! (i - 1)))
         | otherwise = go (i + 1) num

gettingFractional :: (Int, Int) -> (Int, Int)
gettingFractional x@(g, _)
    | even g = forEven x
    | otherwise = forOdd x
    where
        forOdd (g, p) = (g - p + 1, p)
        forEven = swap . forOdd

main :: IO ()
main = (printf "%d/%d" . fst <*> snd) . gettingFractional . findingGroupAndPos =<< readLn

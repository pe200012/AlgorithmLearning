{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import           Data.Maybe  (fromJust)
import           Lens.Micro

findAllShortestPaths :: [[Double]] -> [[Double]]
findAllShortestPaths matrix = go 0 0 0 matrix
    where go k i j (ma :: [[Double]])
            | k == sideLength = ma
            | i == sideLength = go (k + 1) 0 0 ma
            | j == sideLength = go k (i + 1) 0 ma
            | otherwise = go k i (j + 1) $
                if ma ^?! (ix i . ix j) < ma ^?! (ix i . ix k) * ma ^?! (ix k . ix j)
                then ma & (ix i . ix j) .~ (ma ^?! (ix i . ix k) * ma ^?! (ix k . ix j))
                else ma
          sideLength = length matrix

initializeGraph :: Int -> [[Double]]
initializeGraph = replicate <*> flip replicate 1

readExchangeRate :: IO [[Double]]
readExchangeRate = do
    kinds <- readLn
    if kinds == 0
    then return [[]]
    else do
        let matrix = initializeGraph kinds
        currencies <- readCurrency 0 kinds []
        rateLines <- readLn
        readRate 0 rateLines currencies matrix
    where readCurrency index kinds currencies
            | index == kinds = return currencies
            | otherwise = readCurrency (index + 1) kinds . ((: currencies) . (, index)) =<< getLine
          readRate index lines currencies (matrix :: [[Double]])
            | index == lines = return matrix
            | otherwise = do
                [c1, rate, c2] <- words <$> getLine
                let i = fromJust $ lookup c1 currencies
                    j = fromJust $ lookup c2 currencies
                    rateij = read rate
                readRate (index + 1) lines currencies $
                    matrix & (ix i . ix j) .~ rateij

main :: IO ()
main = do
    paths <- findAllShortestPaths <$> readExchangeRate
    print paths
    if any (> 1) [paths !! index !! index|index <- [0..length paths - 1]]
    then putStrLn "Yes"
    else putStrLn "No"

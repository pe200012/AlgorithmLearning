module Main where

import           Control.Monad.ST (runST)
import           Data.Foldable    (for_)
import           Data.STRef       (newSTRef, readSTRef, writeSTRef)
import           Data.Vector      (Vector, fromList, (!), (//))
import           Debug.Trace      (trace)

readStones :: IO [Int]
readStones = do
    n <- readLn
    reverse <$> go 0 n []
    where
        go i n result
         | i == n = return result
         | otherwise = go (i + 1) n . (: result) =<< readLn

generateDPTable :: [Int] -> Vector (Vector Int)
generateDPTable stones = fromList <$> fromList (go [] 0 $ length stones)
    where
        go result i n
         | i == n = result
         | otherwise = go (goColumn [] i 0 n : result)  (i + 1) n
        goColumn result i j n
         | j == n = result
         | otherwise =
             if i == j
             then goColumn (stones !! i : result) i (j + 1) n
             else goColumn (maxBound : result) i (j + 1) n

minimize :: [Int] -> Vector (Vector Int) -> Int
minimize stones dp' = runST $ do
    let sums = scanl (+) 0 stones
        n = length stones
    dp <- newSTRef dp'
    for_ [1..n] $ \len ->
        for_ [0..n - 1] $ \i ->
            let j = min (len + i - 1) (n - 1)
            in for_ [i..j - 1] $ \k -> do
                dpNow <- readSTRef dp
                let currentRow = dpNow ! i
                    mediumRow = dpNow ! (k + 1)
                    current = currentRow ! j
                    medium = currentRow ! k
                    final = mediumRow ! j
                writeSTRef dp (dpNow // [(i, currentRow // [(j, min current (medium + final + sums !! j - sums !! max 0 (i - 1)))])])
    (! (n - 1)) . (! 1) . (trace . show <*> id) <$> readSTRef dp

main :: IO ()
main = do
    stones <- readStones
    print stones
    print $ generateDPTable stones
    print $ (minimize <*> generateDPTable) stones

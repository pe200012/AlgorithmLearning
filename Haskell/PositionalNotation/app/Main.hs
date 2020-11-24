module Main where

import           Control.Monad.ST (runST)
import           Data.Foldable    (for_)
import           Data.Ratio       (Ratio)
import           Data.STRef       (newSTRef, readSTRef, writeSTRef)
import           Data.Vector      (generate, Vector, snoc, (!), (//))
import           Debug.Trace      ()
import Control.Arrow (Arrow((&&&)))

at:: Vector (Vector a) -> (Int, Int) -> a
at vvs (x, y) = vvs ! x ! y

set :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
set vvs (x, y) a = vvs // [(x, (vvs ! x) // [(y, a)])]

over :: Vector (Vector a) -> (Int, Int) -> (a -> a) -> Vector (Vector a)
over vvs (x, y) f = vvs // [(x, (vvs ! x) // [(y, f (vvs `at` (x, y)))])]

solveLinearSystem :: Vector (Vector (Ratio Int)) -> Vector (Ratio Int)  -> Vector (Ratio Int)
solveLinearSystem matrix' b = runST $ do
    let n = length matrix'
    matrix <- newSTRef matrix'
    for_ [0..n - 1] $ \i -> do
        matrixNow <- readSTRef matrix
        writeSTRef matrix (matrixNow // [(i, (matrixNow ! i) `snoc` (b ! i))])
    for_ [0..n - 2] $ \i ->
        for_ [i + 1..n - 1] $ \k -> do
            matrixNow <- readSTRef matrix
            let m_ki = (matrixNow `at` (k, i)) / (matrixNow `at` (i, i))
            for_ [i + 1 .. n] $ \j -> do
                matrixNow <- readSTRef matrix
                writeSTRef matrix ((\m -> set m (k, i) 0) $ over matrixNow (k, j) (subtract (m_ki * (matrixNow `at` (i, j)))))
    for_ [n - 1, n - 2 .. 1] $ \i -> do
        for_ [i - 1, i - 2 .. 0] $ \k -> do
            matrixNow <- readSTRef matrix
            let m_ki = (matrixNow `at` (k, i)) / (matrixNow `at` (i, i))
            writeSTRef matrix ((\m -> set m (k, i) 0) $ over matrixNow (k, n) (subtract (m_ki * (matrixNow `at` (i, n)))))
        matrixNow <- readSTRef matrix
        let m_ii = matrixNow `at` (i, i)
        writeSTRef matrix ((\m -> over m (i, n) (/ m_ii)) $ over matrixNow (i, i) (/ m_ii))
    matrixNow <- readSTRef matrix
    let m_00 = matrixNow `at` (0, 0)
    writeSTRef matrix ((\m -> over m (0, n) (/ m_00)) $ over matrixNow (0, 0) (/ m_00))
    matrixNow <- readSTRef matrix
    return $ generate n (at matrixNow . (id &&& const n))

main :: IO ()
main = pure ()

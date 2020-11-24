module Main where

import           Control.Arrow (Arrow ((&&&)))
import           Lens.Micro    (ix, over, set, (%~), (&), (^?!))

calculatePath :: [[Int]] -> [[Int]] -> Int
calculatePath numtable dptable = go 0 0 dptable !! (columns - 1) !! (rows - 1)
    where
        rows = length numtable
        columns = length (numtable !! 0)
        go :: Int -> Int -> [[Int]] -> [[Int]]
        go y x dptable | y == rows - 1 && x == columns - 1 = dptable
        go y x dptable | x >= columns = go (y + 1) 0 dptable
        go y x dptable | dptable ^?! ix y ^?! ix x == 0 = go y (x + 1) dptable
        go y x dptable = go y (x + 1) $
            dptable & (ix y . ix (x + numtable !! y !! x)) %~ (+ dptable !! y !! x)
                    & (ix (y + numtable !! y !! x) . ix x) %~ (+ dptable !! y !! x)

generateDPTable :: [[Int]] -> [[Int]]
generateDPTable numtable = over (ix 0) (set (ix 0) 1) $ replicate (length numtable) (replicate (length (numtable !! 0)) 0)

readCheckboard :: IO [[Int]]
readCheckboard = do
    rows <- readLn
    if rows == -1
    then return []
    else reverse <$> getEachRow rows []
    where
        getEachRow :: Read a => Int -> [[a]] -> IO [[a]]
        getEachRow 0 board = return board
        getEachRow n board = do
            nums <- fmap (read . pure) <$> getLine
            getEachRow (n - 1) (nums : board)

main :: IO ()
main = do
    (dptable, board) <- (generateDPTable &&& id) <$> readCheckboard
    print $ calculatePath board dptable


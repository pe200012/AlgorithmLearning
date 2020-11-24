module Main where

import Data.Vector (Vector, fromList)

initialize :: IO (Vector Int, Int)
initialize = do
    [_, operations] <- fmap read . words <$> getLine
    nums <- fromList . fmap read . words <$> getLine
    return (nums, operations)

examine :: (a -> a -> Bool) -> Vector a -> Bool
examine p v = undefined

processOperation :: (Vector Int, Int) -> IO ()
processOperation (numbers, operations) = undefined
    where execute index operations
            | index == operations = return ()
            | otherwise = undefined

main :: IO ()
main = pure ()

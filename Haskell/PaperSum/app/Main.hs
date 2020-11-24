{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad       (when)
import           Control.Monad.ST
import           Data.Foldable       (for_)
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.List           (groupBy, partition, sortBy)
import           Data.STRef
import Data.List
import Control.Arrow

readTape :: IO [(Int, Int)]
readTape = do
    getLine
    numbers <- fmap read . words <$> getLine
    colors <- fmap read . words <$> getLine
    return $ zip colors numbers

if' :: Bool -> a -> a -> a
if' b a c = if b then a else c

calculateScore :: [(Int, Int)] -> Int
calculateScore tape = runST $ do
    score <- newSTRef 0
    for_ [3, 5..length tape - 1] $ \len ->
        for_ [0..length tape - len] $ \i ->
            when (fst (tape !! i) == fst (tape !! (i + len - 1))) $
                modifySTRef score ((`mod` 10007) . (+ (2 * i + len + 1) * (snd (tape !! i) + snd (tape !! (i + len - 1)))))
    readSTRef score

-- | return odd colors, even colors (pos, number)
readTape' :: IO (HM.HashMap Int [(Int, Int)], HM.HashMap Int [(Int, Int)])
readTape' = do
    oddm <- newIORef HM.empty
    evenm <- newIORef HM.empty
    tapes <- readTape
    for_ (zip [1..] tapes) $ \(index, (color, number)) -> modifyIORef (if odd index then oddm else evenm) $ \m ->
        HM.alter (\case
                    Just xs -> return $ (index, number) : xs
                    Nothing -> return [(index, number)]
        ) color m
    o <- readIORef oddm
    e <- readIORef evenm
    return (o, e)

calculateScore' :: (HM.HashMap Int [(Int, Int)], HM.HashMap Int [(Int, Int)]) -> Int
calculateScore' (oddm, evenm) = sum (map (sumUp . snd) $ HM.toList oddm) + sum (map (sumUp . snd) $ HM.toList evenm)
    where getSinglePair (p1, n1) (p2, n2) = (p1 + p2) * (n1 + n2)
          sumUp :: [(Int, Int)] -> Int
          sumUp m = sum $ map (uncurry ((sum .) . map . getSinglePair) . first head . splitAt 1 . flip drop m . subtract 1) [1..length m]

main :: IO ()
main = print . calculateScore' =<< readTape'

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns #-}

module Main where


import System.Random (randomRIO, Random(randomIO))
import Control.Monad (replicateM)
import Data.List
import Data.Int (Int8)

a :: IO [Int]
a = do
    n <- randomRIO (1, 100)
    (n:) <$> replicateM n (randomRIO (-1000, 1000))

b :: IO String
b = do
    n <- randomRIO (1, 100)
    xs <- ([n]:) <$> replicateM n a
    return $ intercalate "\r\n" $ unwords . fmap show <$> xs

main :: IO ()
main = do
    x <- b
    writeFile "input.in" x

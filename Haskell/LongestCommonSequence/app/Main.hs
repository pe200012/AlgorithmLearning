{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Vector.Mutable (new, write, modify, read)
import Prelude hiding (replicate, read, Left)
import Control.Monad.ST (runST)
import Data.STRef (modifySTRef, writeSTRef, readSTRef, newSTRef)
import Control.Monad.Loops (whileM_)
import qualified Data.Vector as V (generate, singleton, update, (!))
import Lens.Micro (_2, set)
import qualified Data.Sequence as Seq ((<|), singleton, viewl, ViewL(..), empty)
import Debug.Trace (trace)
import Data.Foldable (Foldable(toList))

data Reference = Up
               | Left
               | Both
               | Upperleft
               | Init
               deriving (Show)

findingLCS ::  String -> String -> [String]
findingLCS "" _ = [""]
findingLCS _ "" = [""]
findingLCS a b = runST $ do
    table <- new (length a + 1)
    index <- newSTRef 0
    whileM_ ((<= length a) <$> readSTRef index) $ do
        id <- readSTRef index
        write table id (V.generate (length b + 1) (\i -> ((id, i), Init)))
        modifySTRef index (+ 1)
    aIndex <- newSTRef 1
    bIndex <- newSTRef 1
    path <- newSTRef Seq.empty
    singleResult <- newSTRef ""
    results <- newSTRef []
    whileM_ ((<= length a) <$> readSTRef aIndex) $ do
        writeSTRef bIndex 1
        whileM_ ((<= length b) <$> readSTRef bIndex) $ do
            i <- readSTRef aIndex
            j <- readSTRef bIndex
            if a !! (i - 1) == b !! (j - 1)
            then do
                x <- set _2 Upperleft <$> ((V.! (j - 1)) <$> read table (i - 1))
                flip (modify table) i (\v -> V.update v $ V.singleton (j, x))
            else do
                n <- (V.! (j - 1)) <$> read table i
                m <- (V.! j) <$> read table (i - 1)
                flip (modify table) i (\v -> V.update v $ V.singleton (j, case compare (fst n) (fst m) of
                                                                            LT -> set _2 Up m
                                                                            GT -> set _2 Left n
                                                                            EQ -> set _2 Both n))
            modifySTRef bIndex (+ 1)
        modifySTRef aIndex (+ 1)
    terminal <- (V.! (length b)) <$> read table (length a)
    writeSTRef path (Seq.singleton (terminal))
    whileM_ (not . null <$> readSTRef path) $ do
        (((x, y), ref) Seq.:< rest) <- Seq.viewl <$> readSTRef path
        trace (show (x,y)) (return ())
        case ref of
            Init -> readSTRef singleResult >>= \r -> modifySTRef results (r :) >> writeSTRef singleResult ""
            Up -> (V.! x) <$> read table (y - 1) >>= \node -> writeSTRef path (node Seq.<| rest)
            Left -> (V.! (x - 1)) <$> read table y >>= \node -> writeSTRef path (node Seq.<| rest)
            Both -> (V.! x) <$> read table (y - 1) >>= \node -> ((V.! (x - 1)) <$> read table y) >>= \node2 -> writeSTRef path (node Seq.<| node2 Seq.<| rest)
            Upperleft -> do
                node <- (V.! (x - 1)) <$> read table (y - 1)
                writeSTRef path (node Seq.<| rest)
                modifySTRef singleResult ((b !! y) :)
    readSTRef results


main :: IO ()
main = pure ()

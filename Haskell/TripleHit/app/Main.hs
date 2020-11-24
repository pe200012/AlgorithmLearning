module Main where

import           Data.Foldable (for_)
import           Data.List     (sortBy, permutations)
import           System.IO     (IOMode (WriteMode), hClose, openFile)
import           Text.Printf   (hPrintf)

num :: [(Int, Int, Int)]
num = go <$> permutations "123456789"
    where go str = let (a, rest1) = splitAt 3 str
                       (b, c) = splitAt 3 rest1
            in (read a, read b, read c)

main :: IO ()
main = do
    handle <- openFile "output.out" WriteMode
    let results = filter (\(a, b, c) -> fromIntegral b / fromIntegral a == 2 && fromIntegral c / fromIntegral a == 3) num
    for_ (sortBy (\(a1, _, _) (a2, _, _) -> compare a1 a2) results) $ \(a, b, c) ->
        hPrintf handle "%d %d %d\n" a b c
    hClose handle

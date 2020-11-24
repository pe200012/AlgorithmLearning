module Main where

import Control.Arrow (Arrow((&&&)))

table :: [Int]
table = [2..36000]

selectAssistant :: [Int] -> [Int] -> [Int]
selectAssistant result [] = result
selectAssistant result (a:rest) = selectAssistant (a:result) (fmap snd $ filter ((/= 0) . (`mod` a) . fst) (zip [1..] rest))

main :: IO ()
main = print $ table'
    where table' = reverse $ selectAssistant [] table
          go = do
              n <- readLn
              if n == 0
              then return ()
              else do
                  print $ table' !! (n - 1)
                  go

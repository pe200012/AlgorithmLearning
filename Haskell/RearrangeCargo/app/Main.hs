module Main where

calculateInversion :: [Int] -> Int
calculateInversion xs = sum $ at <$> [0..length xs - 1]
    where at i = sum $ (\x -> if x > (xs !! i) then 1 else 0) <$> take i xs

readNumber :: IO [Int]
readNumber = do
    len <- readLn
    go len []
    where go len result
            | length result == len = return result
            | otherwise = go len . (result ++) . fmap read . words =<< getLine

main :: IO ()
main = print . calculateInversion =<< readNumber

module Main where

readISBN :: IO (String, Int)
readISBN = do
    str <- words . fmap (\c -> if c == '-' then ' ' else c) <$> getLine
    return (foldl1 (++) (take (length str - 1) str), if last str == "X" then 10 else read $ last str)

showISBN :: (String, Int) -> String
showISBN (xs, iden) = (a ++ "-" ++ b ++ "-" ++ c ++ "-") ++ if iden == 10 then "X" else show iden
    where (a, rest1) = splitAt 1 xs
          (b, c) = splitAt 3 rest1

main :: IO ()
main = do
    (xs, identityNumber) <- readISBN
    let result = (`mod` 11) . sum $ zipWith (*) [1..] (read . pure <$> xs)
    if identityNumber == result
    then putStrLn "Right"
    else putStrLn $ showISBN (xs, result)

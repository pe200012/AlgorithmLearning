module Main where

hanoi3 :: Integer -> Integer
hanoi3 = floor . (subtract 1) . (2 **) . fromIntegral

hanoi4 :: [Int] -> [Int] -> [Int] -> [Int] -> Int
hanoi4 [] [] [] _ = 0
hanoi4 a  b  c  d = undefined

hanoi :: Int -> Integer -> Integer
hanoi 0 _ = undefined
hanoi 1 _ = undefined
hanoi 2 _ = undefined
hanoi 3 x = hanoi3 x
hanoi n 0 = 0
hanoi n 1 = 1
hanoi n x = 2 * hanoi n k + hanoi (n - 1) (x - k)
    where k = (x - ceiling (sqrt . fromIntegral $ 2 * x - 1) + 1)

main :: IO ()
main = do
    print $ hanoi3 10

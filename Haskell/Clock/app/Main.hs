{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.List   (sortBy)
import           Lens.Micro  ((&), (^.))
import           Text.Parsec (space, sepBy, runParserT, ParsecT, Stream, char, count, digit)

data Time = Time (String, Int, Int) deriving Show

parseTime :: (Monad m, Stream s m Char) => ParsecT s u m Time
parseTime = do
    hour <- count 2 digit
    char ':'
    minute <- count 2 digit
    return $ Time (hour ++ ":" ++ minute, read hour, read minute)

parseTimeGroup :: (Monad m, Stream s m Char) => ParsecT s u m [Time]
parseTimeGroup = sepBy parseTime space

calculateAngle :: Time -> Double
calculateAngle (Time (_, h, m)) = abs ((fromIntegral (h `mod` 12) + (fromIntegral m) / 60) * 30 - fromIntegral m * 6)

getMedianTime :: [Time] -> String
getMedianTime timeGroup = sortedGroup !! (length timeGroup `div` 2) & (\(Time (a,_,_)) -> a)
    where
        sortedGroup = sortBy (\a -> compare (calculateAngle a) . calculateAngle) timeGroup

dealWithTestCases :: Int -> IO ()
dealWithTestCases 0 = return ()
dealWithTestCases n = do
    x <- either (error . show) id <$> (runParserT parseTimeGroup () "stdin" =<< getLine)
    putStrLn $ getMedianTime x
    dealWithTestCases (n - 1)

main :: IO ()
main = do
    cases <- readLn
    dealWithTestCases cases

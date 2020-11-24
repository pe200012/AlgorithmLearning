module Main where

import qualified Data.Map as Map
import Data.IORef (writeIORef, readIORef, newIORef)

f :: String -> Int -> Int -> Int -> Int
f str i j k = read (go i j str) * read (go j k str)
    where go i j = take (j - i + 1) . drop i

memo :: Ord a => (a -> b) -> IO (a -> IO b)
memo f = do
    me <- newIORef Map.empty
    return $ \a -> do
        meNow <- readIORef me
        case Map.lookup a meNow of
            Just x -> return x
            Nothing -> let result = f a in do
                writeIORef me . Map.insert a result =<< readIORef me
                return result

main :: IO ()
main = pure ()

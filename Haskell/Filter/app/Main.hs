module Main where

import           Lib
import           System.Directory
import           Control.Concurrent             ( threadDelay )
import           System.Environment

mainLoop :: FilePath -> IO ()
mainLoop wdir = do
    categorizeItems wdir
    threadDelay 60000000
    mainLoop wdir

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then return ()
        else do
            setCurrentDirectory (head args)
            mainLoop (args !! 1)

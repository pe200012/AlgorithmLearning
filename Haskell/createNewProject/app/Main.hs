#!/usr/bin/env -S stack runghc --package optparse-applicative --package process --package directory --
module Main where

import           Control.Monad
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.Process

data Project = Project {
    name              :: String,
    passIfTargetExist :: Bool
} deriving Show

parser :: Parser Project
parser = Project
    <$> strOption
        ( long "name"
        <> short 'n'
        <> metavar "TARGET"
        <> help "Project(folder) name"
        )
    <*> switch
        ( long "pass"
        <> short 'p'
        )

createProject :: Project -> IO ()
createProject Project {name = n, passIfTargetExist = p} = do
    -- callProcess "stack" ["new", n]
    (code, _, out) <- readProcessWithExitCode "stack" ["new", n, "./#template#/luogu.hsfiles"] ""
    putStr out
    case code of
       ExitSuccess -> return ()
       ExitFailure i -> if not p
                        then error "Target directory already exists."
                        else putStrLn "Already exists. Skipping..."
    setCurrentDirectory n
    putStrLn "Writing hie configuration file..."
    writeFile "hie.yaml" =<< readCreateProcess (shell "gen-hie") ""

main :: IO ()
main = createProject =<< execParser (info parser fullDesc)

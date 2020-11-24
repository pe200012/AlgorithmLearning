{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent   (forkIO)
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Conduit.Network (serverSettings)
import           Lib                  (droneServiceProvider, srv)
import           Network.JSONRPC      (Ver (..), jsonrpcTCPServer)

main :: IO ()
main = do
    forkIO droneServiceProvider
    runStderrLoggingT $ do
        let ss = serverSettings 10021 "127.0.0.1"
        jsonrpcTCPServer V2 False ss srv

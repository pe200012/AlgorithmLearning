{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Lib where


import           Control.Concurrent             (forkFinally, forkIO,
                                                 newEmptyMVar, putMVar,
                                                 takeMVar)
import           Control.Eff                    (type (<::), Eff, Lifted,
                                                 LiftedBase, lift, runLift)
import           Control.Eff.Coroutine          (Y (..), Yield, runC, yield)
import           Control.Eff.Log                (Log, LogM, Logger, logE,
                                                 runLog, runLogM, stderrLogger)
import           Control.Exception              (bracket, finally)
import           Control.Monad                  (forM, forM_, liftM, mzero,
                                                 void, when)
import           Control.Monad                  (forever)
import           Control.Monad.Cont             (ContT (runContT), callCC)
import           Control.Monad.Fix              (fix)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Logger           (MonadLogger, MonadLoggerIO,
                                                 logError, logInfo,
                                                 runStderrLoggingT)
import qualified Control.Monad.Trans            as Trans (lift)
import           Crypto.Hash.MD5                (hashlazy)
import           Data.Aeson                     (ToJSON (..), Value (..),
                                                 decode, encode, withText)
import           Data.Aeson.QQ                  (aesonQQ)
import           Data.Aeson.Types               (FromJSON (..), Parser,
                                                 emptyArray, parseMaybe)
import           Data.Bits                      ((.|.))
import qualified Data.ByteString.Char8          as StrictBS (append, length,
                                                             pack, unpack)
import           Data.ByteString.Lazy.Char8     (append, snoc, unpack)
import qualified Data.ByteString.Lazy.Char8     as BS (putStrLn, replicate, append, elem, length,
                                                       pack, takeWhile, unpack)
import           Data.Conduit                   (runConduit, (.|))
import qualified Data.Conduit                   as Conduit (yield)
import qualified Data.Conduit.Combinators       as Conduit
import           Data.Conduit.Network           (sinkSocket, sourceSocket)
import           Data.Int                       (Int64)
import           Data.IORef                     (modifyIORef, newIORef,
                                                 readIORef, writeIORef)
import qualified Data.List                      as List (replicate)
import           Data.Maybe                     (catMaybes, fromJust, fromMaybe)
import           Data.UUID.Types                (UUID)
import           Data.Vector                    (replicate, (//))
import           GHC.Generics                   (Generic)
import           Network.JSONRPC                (BatchRequest (BatchRequest, SingleRequest),
                                                 BatchResponse (BatchResponse),
                                                 FromRequest (..),
                                                 FromResponse (..), JSONRPCT,
                                                 Respond, ToRequest (..),
                                                 Ver (V2), buildResponse,
                                                 jsonrpcTCPServer,
                                                 receiveBatchRequest,
                                                 sendBatchResponse,
                                                 sendResponse)
import           Network.Socket                 (AddrInfo (addrAddress, addrFamily, addrFlags, addrProtocol, addrSocketType),
                                                 AddrInfoFlag (AI_PASSIVE),
                                                 PortNumber, Socket,
                                                 SocketOption (ReuseAddr),
                                                 SocketType (Stream), accept,
                                                 bind, close, defaultHints,
                                                 defaultPort, getAddrInfo,
                                                 gracefulClose, listen,
                                                 setCloseOnExecIfNeeded,
                                                 setSocketOption, socket,
                                                 socketPort, withFdSocket)
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import           Prelude                        hiding (replicate)
import           System.Exit                    (exitSuccess)
import           System.Random                  (randomRIO)

data MCProcedure = WalkingLoop
                 | ShutdownServer

instance FromRequest MCProcedure where
    parseParams "walkingLoop"    = Just . const . pure $ WalkingLoop
    parseParams "shutdownServer" = Just . const . pure $ ShutdownServer
    parseParams _                = Nothing

instance ToRequest MCProcedure where
    requestMethod WalkingLoop    = "walkingLoop"
    requestMethod ShutdownServer = "shutdownServer"
    requestIsNotif ShutdownServer = True
    requestIsNotif _              = False

instance ToJSON MCProcedure where
    toJSON = const emptyArray

data MCProcedureResult = Void
                       | Port PortNumber

instance FromResponse MCProcedureResult where
    parseResult "walkingLoop"    = Just $ (maybe mzero (return . Port . fromInteger) . parseMaybe parseJSON)
    parseResult "shutdownServer" = Just . const . pure $ Void
    parseResult _                = Nothing

instance ToJSON MCProcedureResult where
    toJSON (Port n) = toJSON (toInteger n)
    toJSON _        = emptyArray

data ServerCommand = ServerCommand { method :: String
                                   , params :: Value
                                   }
      deriving (Generic, Show)

instance ToJSON ServerCommand where
instance FromJSON ServerCommand where

data TCornerPosition = A | B | C deriving (Show)

setServer :: Maybe String -> IO Socket
setServer port = resolve >>= open
    where
        mhost = Just "127.0.0.1"
        resolve = do
            let hints = defaultHints {
                    addrFlags = [AI_PASSIVE]
                  , addrSocketType = Stream
                  }
            head <$> getAddrInfo (Just hints) mhost port
        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            withFdSocket sock $ setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 1024
            return sock

droneServiceProvider :: IO ()
droneServiceProvider = do
    server <- setServer (Just "10022")
    forever $ do
        (conn, drone) <- accept server
        forkFinally (guideDrone conn) (const $ gracefulClose conn 5000)

guideDrone :: Socket -> IO ()
guideDrone drone = do
    putStrLn "get a new drone, guiding..."
    inspectscript <- readFile "./luascript/inspect.lua"
    rpcscript <- readFile "./luascripts/rpc.lua"
    dronescript <- readFile "./luascripts/DroneRoute.lua"
    sendSomething inspectscript
    sendSomething rpcscript
    sendSomething dronescript
    where
        sendSomething content = do
            sendAll drone . BS.pack . StrictBS.unpack . hashlazy $ BS.pack content
            recv drone 1
            sendAll drone $ BS.pack content
            x <- read . BS.unpack <$> recv drone 1
            if x == 0
            then return ()
            else do
                putStrLn "md5 check failed, retry..."
                sendSomething content


respond :: MonadIO m => Respond MCProcedure m MCProcedureResult
respond ShutdownServer = liftIO exitSuccess
respond WalkingLoop = do
    portvar <- liftIO newEmptyMVar
    liftIO $ forkIO (singleRun portvar)
    port <- liftIO $ takeMVar portvar
    return . Right $ Port port
    where
        singleRun var =  do
            sock <- setServer Nothing
            p <- socketPort sock
            putMVar var p
            (conn, _peer) <- accept sock
            void $ finally (walkingLoopSession conn) (gracefulClose conn 5000)

srv :: MonadLoggerIO m => JSONRPCT m ()
srv = do
    $(logInfo) "listening for new request"
    qM <- receiveBatchRequest
    case qM of
        Nothing -> do
            $(logInfo) "closed request channel, exting"
            return ()
        Just (SingleRequest q) -> do
            $(logInfo) "got request"
            rM <- buildResponse respond q
            forM_ rM sendResponse
            srv
        Just (BatchRequest qs) -> do
            $(logInfo) "got request batch"
            rs <- catMaybes `liftM` forM qs (buildResponse respond)
            sendBatchResponse $ BatchResponse rs
            srv

walkingLoopSession :: Socket -> IO ()
walkingLoopSession sock = runLift . ((>>= transporting) . runC) $ walkingLoop
    where
        transporting Done = return ()
        transporting (Y c (command :: ServerCommand)) = do
            lift $ putStrLn ("sending.." ++ show command)
            lift $ sendSomething sock (encode command)
            lift $ putStr "reading.."
            r <- lift $ read @Int . unpack <$> readContent sock
            lift $ putStrLn (show r)
            (c r >>= transporting)
        readContent sock = do
            size <- read @Int64 . BS.unpack <$> recv sock 32
            let loop 0 buf = return buf
                loop n buf = do
                    cache <- recv sock n
                    loop (n - BS.length cache) (buf `BS.append` cache)
            loop size ""
        sendSomething sock buf = do
            let x = BS.pack $ show (BS.length buf)
            sendAll sock $ (BS.replicate (32 - (BS.length x)) '0') `BS.append` x
            sendAll sock buf

walkingLoop :: ('[Yield ServerCommand Int] <:: r, Lifted IO r) => Eff r ()
walkingLoop = walking
    where
        dotimes times f args = do
            es <- lift . newIORef $ replicate @Int times 0
            let worker 0 = lift $ foldl (.|.) 0 <$> readIORef es
                worker n = do
                    e <- yield @ServerCommand @Int $
                        ServerCommand {
                            method = f,
                            params = args
                        }
                    currentES <- lift $ readIORef es
                    lift $ writeIORef es (currentES // [(n-1, e)])
                    worker (n - 1)
            worker times
        readBit (n :: Int) = do
            n <- yield @ServerCommand @Int $
                ServerCommand {
                    method = "readBit",
                    params = [aesonQQ|#{n}|]
                }
            return n
        walkForward = do
            n <- yield @ServerCommand @Int $
                ServerCommand {
                    method = "forward",
                    params = Null
                }
            return n
        turnTo direction = do
            e1 <- yield @ServerCommand @Int $
                ServerCommand {
                    method = "forward",
                    params = Null
                }
            e2 <- yield @ServerCommand @Int $
                ServerCommand {
                    method = direction,
                    params = Null
                }
            e3 <- dotimes 4 "forward" Null
            return (e1 .|. e2 .|. e3)
        turnAround = do
            e1 <- yield @ServerCommand @Int $
                ServerCommand {
                    method = "turnAround",
                    params = Null
                }
            e2 <- dotimes 3 "forward" Null
            return (e1 .|. e2)
        robotStop = do
            errno <- yield @ServerCommand @Int $
                ServerCommand {
                    method = "stop",
                    params = Null
                }
            return errno
        tCorner pos = do
            let actionList = case pos of
                                A -> [goRight, goForward]
                                B -> [goLeft, goForward]
                                C -> [goLeft, goRight]
            action <- lift $ (actionList !!) <$> randomRIO (0,1)
            action
            walking
        goCrossing = do
            let actionList = [goLeft, goRight, goForward]
            action <- lift $ (actionList !!) <$> randomRIO (0,2)
            action
            walking
        goLeft = turnTo "turnLeft"
        goRight = turnTo "turnRight"
        goForward = do
            errno <- dotimes 5 "forward" Null
            return errno
        walking = do
            bit <- readBit 1
            case bit of
                0x00 -> walkForward >> walkingLoop
                0x01 -> reading
                0x0F -> stop
        reading = do
            walkForward
            bits <- readBit 2
            case bits of
                0x00 -> goLeft >> walking
                0x01 -> goRight >> walking
                0x02 -> void $ tCorner A -- T A
                0x03 -> void $ tCorner B -- T B
                0x04 -> void $ tCorner C -- T C
                0x05 -> void goCrossing -- Crossing
                0x06 -> turnAround >> walking
                0x07 -> undefined -- Fuel supplies
                0x08 -> undefined -- Charger
        stop = do
            robotStop
            return ()

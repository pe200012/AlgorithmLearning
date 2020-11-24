{-# LANGUAGE TypeOperators, TypeFamilies, AllowAmbiguousTypes, DataKinds, MonoLocalBinds, ScopedTypeVariables, FlexibleContexts, ViewPatterns, TypeApplications #-}

module Tmp (runGame) where

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.Coroutine
import qualified Control.Monad.Trans as Trans
import Control.Monad (when)
import Control.Monad.Cont (ContT, callCC, runCont, runContT)
import Control.Monad.Loops
import Control.Concurrent.MVar
import Data.Maybe (fromJust)
import System.Random

data Request = GetNewNumber
             | InformWrongGuess Ordering

data Response = Void
              | NewNumber Int

randomRE ::('[Reader (MVar StdGen)] <:: r, Lifted IO r, Random a) => (a, a) -> Eff r a
randomRE (lo, hi) = do
    gVar :: MVar StdGen <- ask
    g <- lift $ takeMVar gVar
    let (a, g') = randomR (lo, hi) g
    lift $ putMVar gVar g'
    return a

guess :: ('[Reader (MVar StdGen), Yield Request Response] <:: r, Lifted IO r) => Eff r ()
guess = do
    x :: Int <- randomRE (0, 100)
    (`runContT` (lift . return)) . callCC $ \escape -> whileM_ (pure True) $ do
        res <- Trans.lift $ yield GetNewNumber
        let (NewNumber gu) = res
        when (gu == x) (escape ())
        Trans.lift $ yield @Request @Response (InformWrongGuess (gu `compare` x))

runGame :: IO ()
runGame = do
    g <- getStdGen >>= newMVar
    pre <- newMVar (-1) 
    runLift . ((>>= deal' pre) . runC) . runReader g $ guess
    putStrLn "Good catch!"
    g' <- takeMVar g
    setStdGen g'
    where
        deal Done = return ()
        deal (Y c GetNewNumber) = do
            lift $ putStr "guess: "
            x :: Int <- lift readLn
            (c (NewNumber x)) >>= deal
        deal (Y c (InformWrongGuess ord)) = do
            lift $ putStrLn ("Sorry, " ++ show ord ++ ". Please try again.")
            (c Void) >>= deal
        deal' _ Done = return ()
        deal' p (Y c (InformWrongGuess ord)) = do
            lift $ putStrLn ("Sorry, " ++ show ord ++ ". Please try again.")
            (c Void) >>= deal' p
        deal' previousN (Y c GetNewNumber) = do
            x <- lift (ai previousN)
            (c (NewNumber x)) >>= deal' previousN
        ai previousN = do
            x <- (+1) <$> takeMVar previousN
            print x
            putMVar previousN x
            return x

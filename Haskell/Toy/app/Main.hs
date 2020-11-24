{-# LANGUAGE Arrows              #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Arrow        (Arrow (..), returnA, (>>>))
import           Control.Concurrent   (threadDelay)
import           Control.Lens         (view, _1)
import           Control.Monad        (liftM)
import           Data.IORef           (IORef, newIORef, readIORef, writeIORef)
import           Engine.QuadTree      (Region (..))
import           FRP.Yampa            (Arrow (first), SF, identity)
import           FRP.Yampa.Event      (Event (Event))
import           FRP.Yampa.Simulation (reactimate)
import           SDL                  hiding (Event, identity)
import qualified SDL
import           SDL.Raw.Timer        (getTicks)
import           Type.Environment
import           Type.Event
import           Type.Player

main :: IO ()
main = gameStart

gameStart :: IO ()
gameStart = do
  initializeAll
  time <- newIORef (0 :: Int)
  window <- createWindow "Toy" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  player <- (newPlayer (Region (V2 0 0, V2 10 10)) "./img/BasicBlock.jpg") >>= newIORef
  p <- readIORef player
  es <- pollEvents
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  present renderer
  reactimate (return (es, p, renderer))
             (\_ -> do
               newtime <- fromIntegral <$> getTicks
               previous <- readIORef time
               events <- pollEvents
               p <- readIORef player
               writeIORef time newtime
               return (fromIntegral $ newtime - previous, Just (events, p, renderer))
               )
             (const (\((actions, newplayer), events) -> actions >> writeIORef player newplayer >> (return $ quitPressed $ events)))
             (appLoop &&& arr (view _1))

player1 :: IO (IORef Player)
player1 = (newPlayer (Region (V2 0 0, V2 10 10)) "./img/BasicBlock.jpg") >>= newIORef

map1 :: GameMap
map1 = GameMap (800, 600)

appLoop :: SF ([SDL.Event], Player, Renderer) (IO (), Player)
appLoop = proc (es, p, re) -> do
            mmt <- movement -< es
            p' <- playerMove map1 -< (p, mmt)
            a <- arr (\(p,p',re) -> playerRender (p,p') re) -< (p, p', re)
            b <- arr (\p' -> player1 >>= flip writeIORef p') -< p'
            returnA -< (a >> b >> present re, p')

  -- loop es = do
    -- p <- readIORef player1
    -- p' <- playerMove p map1 -< movement es
    -- return ()

{- main :: IO ()
main = gameStart >> quit

generateFood :: Environment -> IO BasicBlock
generateFood (runGameMap . getMap -> V2 width height) = do
  x <-
    getStdRandom (randomR (0, fromIntegral $ (width - 10) `div` 10))
      >>= (return . (xranges !!))
  y <-
    getStdRandom (randomR (0, fromIntegral $ (height - 10) `div` 10))
      >>= (return . (yranges !!))
  return (BasicBlock $ PointView x y)
 where
  ranges  = [0, 10 ..]
  xranges = takeWhile (<= width - 10) ranges
  yranges = takeWhile (<= height - 10) ranges

gameStart :: IO ()
gameStart = do
  initializeAll
  window   <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  surface  <- getWindowSurface window
  let player   = createNewSnake (BasicBlock $ PointView 10 10)
      env      = Environment (gameMap (800, 600))
      rpayload = RendererPayload $ MkRendererPayload renderer
  initialFood <- generateFood env
  windowSize window $= V2 800 600
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  appLoop
    window
    rpayload
    ( GameLoopPayload
    $ MkGameLoopPayload env DOWN (BasicBlock $ PointView 100 100)
    )
    player

appLoop
  :: Window
  -> Payload 'RendererType
  -> Payload 'GameLoopType
  -> Snake
  -> IO ()
appLoop win rpayload@(RendererPayload (getRenderer -> renderer)) (GameLoopPayload payload) s
  = do
    events <- pollEvents
    let qPressed = any eventIsQPress events
        eventIsQPress event = case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent
              == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent)
              == KeycodeQ
          _ -> False
        exitPressed = any eventIsClosing events
        eventIsClosing event = case eventPayload event of
          WindowClosedEvent _ -> True
          _                   -> False
        previousDirection = getPreviousDirection payload
        currentDirection  = fromJust (userDirection <|> Just previousDirection)
        userDirection     = foldl' (<|>) Nothing $ fmap directionDispatch events
        directionDispatch event = case eventPayload event of
          KeyboardEvent keyboardEvent
            | keyboardEventKeyMotion keyboardEvent == Pressed
            -> case keysymKeycode (keyboardEventKeysym keyboardEvent) of
              KeycodeW | previousDirection /= DOWN -> Just UP
              KeycodeS | previousDirection /= UP -> Just DOWN
              KeycodeA | previousDirection /= RIGHT -> Just LEFT
              KeycodeD | previousDirection /= LEFT -> Just RIGHT
              _ -> Nothing
          _ -> Nothing
        turnGap = 200000 -- one fifth of a second
        s'      = if snakeHead s == getFood payload
          then grow (getEnvironment payload) s currentDirection
          else crawl (getEnvironment payload) s currentDirection
        newFood = if snakeHead s == getFood payload
          then generateFood (getEnvironment payload)
          else return (getFood payload)
    newFood' <- newFood
    erase rpayload s
    erase rpayload (getFood payload)
    render rpayload s'
    render rpayload newFood'
    present renderer
    threadDelay turnGap
    unless
      (qPressed || exitPressed)
      (appLoop
        win
        rpayload
        (GameLoopPayload $ payload { getFood              = newFood'
                                   , getPreviousDirection = currentDirection
                                   }
        )
        s'
      )
 -}

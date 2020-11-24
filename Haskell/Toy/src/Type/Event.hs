{-# LANGUAGE Arrows #-}

module Type.Event (Direction(..), movement, quitPressed) where

import           Control.Applicative (Alternative ((<|>)))
import           Data.List           (foldl')
import           FRP.Yampa           (SF, arr)
import           FRP.Yampa.Basic     (constant)
import           FRP.Yampa.Event     (Event (..), maybeToEvent)
import           SDL                 hiding (Event)
import qualified SDL
import           SDL.Event           (EventPayload (..))

data Direction = UP
               | DOWN
               | LEFT
               | RIGHT

movement :: SF [SDL.Event] (Event Direction)
movement =
    arr (maybeToEvent . foldr (<|>) Nothing . fmap dispatch)
    where dispatch event = case eventPayload event of
                               KeyboardEvent keyboardEvent
                                 | keyboardEventKeyMotion keyboardEvent == Pressed
                                 -> case keysymKeycode (keyboardEventKeysym keyboardEvent) of
                                   KeycodeW -> Just UP
                                   KeycodeS -> Just DOWN
                                   KeycodeA -> Just LEFT
                                   KeycodeD -> Just RIGHT
                                   _        -> Nothing
                               _ -> Nothing

quitPressed :: [SDL.Event] -> Bool
quitPressed =
    or . fmap dispatch
    where dispatch event = case eventPayload event of
                               KeyboardEvent keyboardEvent
                                 | keyboardEventKeyMotion keyboardEvent == Pressed
                                 -> case keysymKeycode (keyboardEventKeysym keyboardEvent) of
                                   KeycodeQ -> True
                                   _        -> False
                               WindowClosedEvent _ -> True
                               _ -> False

{-# LANGUAGE ViewPatterns #-}

module Type.Environment (GameMap(..)) where

import           Control.Lens
import           Engine.CollisionDetection (Spatial (..), SpatialData (..))
import           SDL.Vect                  (V2 (V2))

data GameMap = GameMap (Int, Int)

instance Spatial GameMap where
    getSpatialData (GameMap (over both fromIntegral -> (w,h))) = OBB [V2 0 0, V2 w 0, V2 w h, V2 0 h]

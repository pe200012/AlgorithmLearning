{-# LANGUAGE Arrows       #-}
{-# LANGUAGE ViewPatterns #-}
module Type.Player (Player, newPlayer, playerMove, playerRender) where

import           Control.Arrow             (returnA, (>>>))
import           Control.Lens              hiding (contains)
import           Control.Monad             (liftM)
import           Engine.CollisionDetection
import           Engine.Manager
import           Engine.QuadTree           (Region (..))
import           Foreign.C.Types           (CInt (CInt))
import           FRP.Yampa                 (SF, arr)
import           FRP.Yampa.Event           (Event, event)
import           Linear.V2                 (V2 (V2))
import           Linear.V4                 (V4 (V4))
import           SDL                       (Point (..), Rectangle (..),
                                            Renderer, Surface, Texture, clear,
                                            copy, createTextureFromSurface,
                                            drawRect, fillRect, get,
                                            rendererDrawColor, ($=))
import           SDL.Image                 (load)
import           Type.Environment
import           Type.Event

data Player = Player { getPos  :: Region
                     , getFace :: Surface
                     }

instance Spatial Player where
      getSpatialData = getSpatialData . getPos

instance GameObjectLike Player where

newPlayer :: Region -> FilePath -> IO Player
newPlayer r f = liftM (Player r) (load f)

move :: Player -> Direction -> Player
move p@(getPos -> Region (V2 x y, r)) UP = p { getPos = Region (V2 x (y - 1), r) }
move p@(getPos -> Region (V2 x y, r)) DOWN = p { getPos = Region (V2 x (y + 1), r) }
move p@(getPos -> Region (V2 x y, r)) LEFT = p { getPos = Region (V2 (x - 1) y, r) }
move p@(getPos -> Region (V2 x y, r)) RIGHT = p { getPos = Region (V2 (x + 1) y, r) }

playerMove :: GameMap -> SF (Player, Event Direction) Player
playerMove m = proc (p, e) -> do
                p' <- arr (uncurry (event <*> move)) -< (p, e)
                returnA -< if p' `exceedBoundry` m
                           then p
                           else p'

playerRender :: Player -> Renderer -> IO ()
playerRender new@(getPos -> Region (over both (fmap (CInt . fromIntegral)) -> (P -> p', r'))) re = do
      rendererDrawColor re $= V4 255 255 255 255
      clear re
      face <- createTextureFromSurface re (getFace new)
      copy re face Nothing (Just (Rectangle p' r'))

exceedBoundry :: Player -> GameMap -> Bool
exceedBoundry p = not . flip contains p

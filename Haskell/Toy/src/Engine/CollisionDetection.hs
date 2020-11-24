{-# LANGUAGE ViewPatterns #-}

module Engine.CollisionDetection
  ( SpatialData(..)
  , pointsTo
  , isAtLeftHandSideOf
  , isAtRightHandSideOf
  , Spatial(..)
  , InternalPoint
  , Segment
  , getEdges
  , covers
  , contains
  , intersectWith
  ) where

import           Linear.Metric (distance, norm, project)
import           Linear.V2     (V2 (..), crossZ, perp)

type InternalPoint = V2 Double

type Segment = (InternalPoint, InternalPoint)

data SpatialData
  = OBB [InternalPoint]
  | Circle (InternalPoint, Double)

class Spatial a where
  getSpatialData :: a -> SpatialData

pointsTo :: InternalPoint -> InternalPoint -> V2 Double
pointsTo = flip (-)

isAtLeftHandSideOf :: InternalPoint -> Segment -> Bool
p `isAtLeftHandSideOf` (s, e) = crossZ (s `pointsTo` p) (s `pointsTo` e) < 0

isAtRightHandSideOf :: InternalPoint -> Segment -> Bool
p `isAtRightHandSideOf` (s, e) = crossZ (s `pointsTo` p) (s `pointsTo` e) > 0

getEdges :: [InternalPoint] -> [Segment]
getEdges [] = []
getEdges [x] = []
getEdges x@(a:_:_) = go (x ++ [a]) []
  where
    go [] c             = c
    go [_] c            = c
    go (x:rest@(y:_)) c = go rest ((x, y) : c)

covers :: (Spatial a) => a -> InternalPoint -> Bool
(getSpatialData -> Circle (centre, radius)) `covers` p =
  distance centre p < radius
(getSpatialData -> OBB (getEdges -> edges)) `covers` p@(V2 x0 y0) =
  (/= 0) . sum $
  map
    (\s@(V2 x1 y1, V2 x2 y2) ->
       if y1 <= y0
         then (if y2 > y0 && p `isAtLeftHandSideOf` s
                 then 1
                 else 0)
         else (if y2 <= y0 && p `isAtRightHandSideOf` s
                 then (-1)
                 else 0))
    edges

contains :: (Spatial a, Spatial b) => a -> b -> Bool
(getSpatialData -> Circle (centre, radius)) `contains` (getSpatialData -> Circle (centre', radius')) =
  distance centre centre' < abs (radius - radius')
a@(getSpatialData -> Circle _) `contains` (getSpatialData -> OBB vertice) =
  and (covers a <$> vertice)
(getSpatialData -> OBB vertice) `contains` b@(getSpatialData -> Circle _) =
  and (not . covers b <$> vertice)
a `contains` (getSpatialData -> OBB vertice) = and (covers a <$> vertice)

intersectWith :: (Spatial a, Spatial b) => a -> b -> Bool
(getSpatialData -> Circle ((V2 x1 y1), radius)) `intersectWith` (getSpatialData -> Circle ((V2 x2 y2), radius')) =
  (radius + radius') ^ 2 >= (x1 - x2) ^ 2 + (y1 - y2) ^ 2
(getSpatialData -> Circle (centre, radius)) `intersectWith` (getSpatialData -> OBB vertice) =
  (< radius) . minimum $ distance centre <$> vertice
b@(getSpatialData -> OBB _) `intersectWith` a@(getSpatialData -> Circle _) =
  a `intersectWith` b
(getSpatialData -> OBB vertice) `intersectWith` (getSpatialData -> OBB vertice') =
  and (zipWith overlap (projection vertice) (projection vertice'))
  where
    axes = perp . uncurry pointsTo <$> getEdges vertice
    projection vs =
      map
        (\axis ->
           let eps = map (norm . project axis) vs
            in (minimum eps, maximum eps))
        axes
    overlap (l1, h1) (l2, h2) = h1 >= l2 && l1 <= h2

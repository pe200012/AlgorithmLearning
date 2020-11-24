
module Coordinate where

import           Control.Exception (assert)
import           Data.List         (find)
import           Lens.Micro        ((^.))
import           Linear            (Metric (distance))
import           Linear.V2         (R1 (..), V2 (..))

type Point = V2 Double
type Distance = Double

data DistanceData2D = DistanceData2D (Point, Distance) (Point, Distance) (Point, Distance) deriving Show

-- | Calculate
calculate2DCoordinates :: DistanceData2D -> Point
calculate2DCoordinates (DistanceData2D a@((V2 x1 y1), r1) b@((V2 x2 y2), r2) c@((V2 x3 y3), r3))
    | isTangent a b = let rr = r1 / (r1 + r2)
                          x = x1 + (x2 - x1) * rr
                          y = y1 + (y2 - y1) * rr
                      in assert (distance (V2 x y) (V2 x3 y3) == r3) V2 x y
    | isTangent c b = let rr = r3 / (r3 + r2)
                          x = x3 + (x2 - x3) * rr
                          y = y3 + (y2 - y3) * rr
                      in assert (distance (V2 x y) (V2 x1 y1) == r1) V2 x y
    | isTangent a c = let rr = r1 / (r1 + r3)
                          x = x1 + (x3 - x1) * rr
                          y = y1 + (y3 - y1) * rr
                      in assert (distance (V2 x y) (V2 x2 y2) == r2) V2 x y
    | otherwise = let distanceSquareAB = (x1 - x2) ** 2 + (y1 - y2) ** 2
                      distanceAB = sqrt distanceSquareAB
                      intermediaSegmentAE = (r2 ** 2 - r1 ** 2 - distanceSquareAB)/ (-2 * distanceAB)
                      intermediaSegmentCE = sqrt (r1 ** 2 - intermediaSegmentAE ** 2)
                      xe = x1 + ((x2 - x1) * intermediaSegmentAE) / distanceAB
                      ye = y1 + ((y2 - y1) * intermediaSegmentAE) / distanceAB
                      -- | may be zero
                      slopeAB = (y2 - y1) / (x2 - x1)
                      slopeCD = (-1) / slopeAB
                      angleCDX = atan slopeCD
                      -- | two possible points
                      α = V2 (xe + intermediaSegmentCE * cos angleCDX) (ye + intermediaSegmentCE * sin angleCDX)
                      β = V2 (xe - intermediaSegmentCE * cos angleCDX) (ye - intermediaSegmentCE * sin angleCDX)
                  in case find ((goodEnough 0.1 r3) . distance (V2 x3 y3)) [α, β] of
                      Just x  -> x
                      Nothing -> error "impossible"
    where isTangent (V2 xa ya, ra) (V2 xb yb, rb) = goodEnough 0.1 ((xa - xb) ** 2 + (ya - yb) ** 2) ((ra + rb) ** 2)
          goodEnough delta a b = abs (a - b) <= delta

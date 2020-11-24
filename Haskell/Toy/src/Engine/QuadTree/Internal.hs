{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

module Engine.QuadTree.Internal
  ( AbstractObject(..)
  , QuadTree(..)
  , Region (..)
  , _region
  , _objects
  , createQuadTreeAt 
  , insert
  , delete
  , update
  , lookupArea
  , detectCollision)
 where

import           Control.Lens              hiding (contains, has)
import           Data.List                 (intercalate)
import           Data.UUID.Types           (UUID (..))
import           Engine.CollisionDetection (Spatial (..), SpatialData (OBB),
                                            contains, covers, intersectWith,
                                            pointsTo)
import           Linear                    (V2 (..))
import           Linear.Vector             (scaled)
import           System.Random             (Random (randomIO))

data AbstractObject =
  forall a. Spatial a =>
            AbstractObject UUID a

instance Spatial AbstractObject where
  getSpatialData (AbstractObject _ x) = getSpatialData x

instance Eq AbstractObject where
  (AbstractObject id _) == (AbstractObject id' _) = id == id'

instance Show AbstractObject where
  show (AbstractObject id _) =
    "AbstractObject " ++ show id ++ " <Opaque Object>"

newtype Region =
  Region (V2 Int, V2 Int)
  deriving (Show, Eq)

instance Spatial Region where
  getSpatialData (Region (topleft, rect@(scaled -> V2 w h))) =
    OBB $
    (fmap fromIntegral) <$> [topleft, topleft + w, topleft + rect, topleft + h]

data QuadTree = Leaf Region [AbstractObject]
              | Quadrants { p  :: V2 Int
                          , nw :: QuadTree
                          , ne :: QuadTree
                          , sw :: QuadTree
                          , se :: QuadTree
                          }
      deriving (Show)

_region :: Lens' QuadTree Region
_region =
  lens
    (\case
       (Leaf r _) -> r
       (Quadrants _ a _ _ b) ->
         let (Region (topleft, _)) = a ^. _region
             (Region (centre, rect)) = b ^. _region
          in Region (topleft, centre - topleft + rect))
    (\t r0 ->
       case t of
         Leaf _ xs    -> Leaf r0 xs
         Quadrants {} -> error "Not implemented")

_objects :: Lens' QuadTree [AbstractObject]
_objects =
  lens
    (\case
       (Leaf _ xs) -> xs
       (Quadrants _ a b c d) -> concatMap (view _objects) [a, b, c, d])
    (\t xs ->
       case t of
         Leaf r _ -> Leaf r xs
         Quadrants {} ->
           let new_t = createQuadTreeAt (t ^. _region)
            in foldr insert new_t xs)

smallEnough :: Region -> Bool
smallEnough (Region (V2 _ _, V2 w h)) = w <= 10 || h <= 10

pattern IsLeaf :: QuadTree

pattern IsLeaf <- Leaf _ _

pattern IsQuadrants :: QuadTree

pattern IsQuadrants <- Quadrants _ _ _ _ _

pattern SmallEnough :: QuadTree

pattern SmallEnough <- (Leaf (smallEnough -> True) _)

createQuadTreeAt :: Region -> QuadTree
createQuadTreeAt r = Leaf r []

subdivide :: QuadTree -> QuadTree
subdivide (Leaf (Region (topleft@(V2 x0 y0), V2 w h)) xs) =
  foldr
    insert
    (Quadrants
       centre
       (createQuadTreeAt (Region (topleft, rectTL)))
       (createQuadTreeAt (Region (top, rectT)))
       (createQuadTreeAt (Region (left, rectL)))
       (createQuadTreeAt (Region (centre, rectC))))
    xs
  where
    top = topleft + w1
    left = topleft + h1
    centre = (`div` 2) <$> bottomRight
    right = centre + w2
    bottom = centre + h2
    bottomRight = (V2 (x0 + w) (y0 + h))
    rectTL@(scaled -> V2 w1 h1) = centre - topleft
    rectC@(scaled -> V2 w2 h2) = bottomRight - centre
    rectT = right - top
    rectL = bottom - left
subdivide q@Quadrants {} = q

insert :: AbstractObject -> QuadTree -> QuadTree
insert x t
  | not (x `intersectWith` (t ^. _region)) = t
insert x l@SmallEnough = l & _objects %~ (x :)
insert x l@IsLeaf
  | x `contains` (l ^. _region) = l & _objects %~ (x :)
  | otherwise = insert x (subdivide l)
insert x (Quadrants p a b c d) =
  Quadrants p (insert x a) (insert x b) (insert x c) (insert x d)

delete :: AbstractObject -> QuadTree -> QuadTree
delete x t
  | not (x `intersectWith` (t ^. _region)) = t
delete x l@IsLeaf = l & _objects %~ (filter (/= x))
delete x (Quadrants p a b c d) =
  Quadrants p (delete x a) (delete x b) (delete x c) (delete x d)

update :: AbstractObject -> QuadTree -> QuadTree
update x = insert x . delete x

lookupArea :: AbstractObject -> QuadTree -> [QuadTree]
lookupArea x t
  | not (x `intersectWith` (t ^. _region)) = []
lookupArea x l@IsLeaf = [l]
lookupArea x q@(Quadrants _ a b c d)
  | x `contains` (q ^. _region) = [q]
  | otherwise = concatMap (lookupArea x) [a, b, c, d]

detectCollision :: AbstractObject -> QuadTree -> [AbstractObject]
detectCollision x t
  | not (x `intersectWith` (t ^. _region)) = []
  | x `contains` (t ^. _region) = t ^. _objects
detectCollision x l@IsLeaf = l ^. _objects & filter (intersectWith x)
detectCollision x q@(Quadrants _ a b c d) =
  concatMap (detectCollision x) [a, b, c, d]

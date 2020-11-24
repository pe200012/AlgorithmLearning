{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Engine.QuadTree where

import           Control.Lens
import           Data.Extensible
import           Data.UUID.Types          (UUID)
import qualified Data.UUID.Types as UUID
import qualified Engine.QuadTree.Internal as Internal
import qualified Type.General             as General (SpatialType (..))
import Engine.CollisionDetection
import Linear.V2 (V2(V2))

spatialHelper :: forall a fs. (Spatial a, Lookup fs "spatial" General.SpatialType, _) => Record fs -> a
spatialHelper x = case x ^. #spatial of General.SpatialType a -> a

insert :: (Lookup fs "id" UUID, Lookup fs "spatial" General.SpatialType) => Record fs -> Internal.QuadTree -> Internal.QuadTree
insert r t = Internal.insert (Internal.AbstractObject (r ^. #id) (spatialHelper r)) t

delete :: (Lookup fs "id" UUID, Lookup fs "spatial" General.SpatialType) => Record fs -> Internal.QuadTree -> Internal.QuadTree
delete r t = Internal.delete (Internal.AbstractObject (r ^. #id) (case r ^. #spatial of General.SpatialType x -> x)) t

update :: (Lookup fs "id" UUID, Lookup fs "spatial" General.SpatialType) => Record fs -> Internal.QuadTree -> Internal.QuadTree
update r t = Internal.update (Internal.AbstractObject (r ^. #id) (case r ^. #spatial of General.SpatialType x -> x)) t

foo :: Record '["id" >: UUID, "spatial" >: General.SpatialType]
foo = #id @= UUID.nil
    <: #spatial @= (General.SpatialType (Internal.Region (V2 0 0,V2 10 10)))
    <: nil

t :: Internal.QuadTree
t = Internal.createQuadTreeAt (Internal.Region (V2 0 0, V2 10 10))

t' :: Internal.QuadTree
t' = insert foo t

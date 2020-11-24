{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Type.General (AbstractObject(..), IdentityObject(..), ExecutableType(..), SpatialType(..), RenderableType(..)) where

import           Control.Lens              ()
import           Data.Extensible
import           Data.Functor.Identity     (Identity (Identity))
import           Data.UUID.Types           (UUID)
import qualified Data.UUID.Types           as UUID
import           Engine.CollisionDetection (Spatial (..))
import           FRP.Yampa                 (SF)
import           Linear.V2                 (V2 (V2))
import qualified SDL
import           SDL.Video.Renderer        (Renderer)

class Executable a where
    execute :: a -> Manager -> a

class Renderable a where
    render :: Renderer -> a -> Manager -> IO ()

data ExecutableType = forall a. Executable a => ExecutableType a
data SpatialType = forall a. Spatial a => SpatialType a
data RenderableType = forall a. Renderable a => RenderableType a

type IdentityObject = '[ "id" >: UUID, "method" >: ExecutableType]
type AbstractObject = IdentityObject ++ '[ "spatial" >: SpatialType ]
type RenderableObject = AbstractObject ++ '[ "renderMethod" >: RenderableType ]

data Manager = Manager { getActions           :: IO ()
                       , getRenderer          :: Renderer
                       , getObjects           :: [Record IdentityObject]
                       , getRenderQueue       :: [Record RenderableObject]
                       , getCurrentInputEvent :: [SDL.Event]
                       }

createIdentityObject :: forall a. Executable a => UUID -> a -> Record IdentityObject
createIdentityObject id x = #id @= id
                         <: #method @= (ExecutableType x)
                         <: nil

createAbstractObject :: forall a. (Spatial a, Executable a) => UUID -> a -> Record AbstractObject
createAbstractObject id x = #id @= id
                         <: #method @= (ExecutableType x)
                         <: #spatial @= (SpatialType x)
                         <: nil

createRenderableObject :: forall a. (Renderable a, Spatial a, Executable a) => UUID -> a -> Record RenderableObject 
createRenderableObject id x = #id @= id
                           <: #method @= (ExecutableType x)
                           <: #spatial @= (SpatialType x)
                           <: #renderMethod @= (RenderableType x)
                           <: nil

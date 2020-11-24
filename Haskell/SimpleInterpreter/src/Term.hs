{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Term where

import           Control.Lens             (makeLenses, over, view)
import           Control.Monad.Except     (MonadError, throwError)
import           Control.Monad.State.Lazy (MonadState, get, modify)
import           Data.Map                 (Map, empty, insert, lookup)
import           Prelude                  hiding (lookup)
import           Text.Printf              (printf)

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance (Show (f (Fix f))) => Show (Fix f)

newtype Mu f = Mu { unMu :: forall a. (f a -> a) -> a }

data TermF a x = LitF    Double
               | VarF    String
               | AssignF String x
               | AddF    x x
               | SubF    x x
               | MulF    x x
               | DivF    x x
               | ModF    x x
               | NegF    x
               deriving (Show, Eq, Functor, Foldable, Traversable)

type Term a = Fix (TermF a)

type Result = Maybe Double

instance Num (Term Double) where
  (+) = addf
  (-) = subf
  (*) = mulf
  fromInteger = litf . fromIntegral
  abs = error "abs not implemented"
  signum = error "signum not implemented"

varf :: String -> Term Double
varf = Fix . VarF

litf :: Double -> Term Double
litf = Fix . LitF

addf :: Term a -> Term a -> Term a
addf a = Fix . AddF a

subf :: Term a -> Term a -> Term a
subf a = Fix . SubF a

mulf :: Term a -> Term a -> Term a
mulf a = Fix . MulF a

divf :: Term a -> Term a -> Term a
divf a = Fix . DivF a

modf :: Term a -> Term a -> Term a
modf a = Fix . DivF a

negf :: Term a -> Term a
negf = Fix . NegF

assignf :: String -> Term a -> Term a
assignf s = Fix . AssignF s

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

cataM :: (Monad m, Traversable t) => (t a -> m a) -> Fix t -> m a
cataM f = (f =<<) . mapM (cataM f) . unFix

pattern IsJust :: Result
pattern IsJust <- Just _

eval :: (MonadError String m, MonadState Interpreter m) => m (Term Double) -> m Double
eval = (>>= cataM phi) where
  phi (LitF d) = return d
  phi (VarF s) = do
    v <- lookup s <$> get
    case v of
      Nothing -> throwError $ printf "ERROR: Invalid identifier. No variable with name '%s' was found." s
      Just d -> return d
  phi (AssignF s a) = modify (insert s a) >> return a
  phi (AddF a b) = return (a + b)
  phi (SubF a b) = return (a - b)
  phi (MulF a b) = return (a * b)
  phi (DivF a b) = return (a / b)
  phi (ModF a b) = return . fromIntegral $ (floor a) `mod` (floor b)
  phi (NegF a)   = return (- a)


type Interpreter = Map String Double

newInterpreter :: Interpreter
newInterpreter = empty

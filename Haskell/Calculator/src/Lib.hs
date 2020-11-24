{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import           Control.Monad.Except   (ExceptT, MonadError, throwError)
import           Control.Monad.Identity (Identity)
import           Text.Parsec            (Parsec, ParsecT, anyChar, char, digit,
                                         getState, many, modifyState,
                                         notFollowedBy, option, optionMaybe,
                                         parse, runParser, spaces, try,
                                         unexpected, (<?>), (<|>))
newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Eq (f (Fix f))) => Eq (Fix f)

newtype Mu f = Mu { unMu :: forall a. (f a -> a) -> a }

data TermF a x = LitF    Double
               | AddF    x x
               | SubF    x x
               | MulF    x x
               | DivF    x x
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

negf :: Term a -> Term a
negf = Fix . NegF

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

cataM :: (Monad m, Traversable t) => (t a -> m a) -> Fix t -> m a
cataM f = (f =<<) . mapM (cataM f) . unFix

pattern IsJust :: Result
pattern IsJust <- Just _

eval :: Monad m => Term Double -> ExceptT String m Double
eval = cataM phi where
  phi (LitF d) = return d
  phi (AddF a b) = return (a + b)
  phi (SubF a b) = return (a - b)
  phi (MulF a b) = return (a * b)
  phi (DivF a b)
    | b == 0 = throwError "Divide by zero"
    | otherwise = return (a / b)
  phi (NegF a)   = return (- a)

type TermParser = Parsec String () (Term Double)

parseSign :: Parsec String u (Term a -> Term a)
parseSign = do
  s <- optionMaybe (try (char '-') <|> try (char '+'))
  return $ case s of
    Just '-' -> negf
    Just '+' -> id
    Nothing  -> id

parseLit :: TermParser
parseLit = do
  coefficient <- parseSign
  n <- fromIntegral . read @Int <$> many digit
  frac <- option 0 parseFrac
  return . coefficient . litf $ n + frac
  where
    parseFrac = do
      char '.'
      s <- many digit
      if null s
      then return 0
      else return (read s / (10 ** (fromIntegral $ length s)))

{- expr :: TermParser
expr     = buildExpressionParser table term
        <?> "expression"

term :: TermParser
term    =  parens expr
        <|> parseLit
        <?> "simple expression"

table :: [[Operator String () Identity (Term Double)]]
table   = [ [prefix "-" negf, prefix "+" id ]
          , [binary "*" mulf AssocLeft, binary "/" divf AssocLeft ]
          , [binary "+" addf AssocLeft, binary "-" subf AssocLeft ]
         ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix :: String -> (a -> a) -> Operator String u Identity a
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix :: String -> (a -> a) -> Operator String u Identity a
postfix name fun       = Postfix (do{ reservedOp name; return fun })
 -}
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint (Term Double) where
  prettyPrint = cata phi where
    phi (LitF x)   = show x
    phi (AddF x y) = "(" ++ x ++ "+" ++ y ++ ")"
    phi (SubF x y) = "(" ++ x ++ "-" ++ y ++ ")"
    phi (MulF x y) = "(" ++ x ++ "*" ++ y ++ ")"
    phi (DivF x y) = "(" ++ x ++ "/" ++ y ++ ")"
    phi (NegF x)   = "-" ++ x

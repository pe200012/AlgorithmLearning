{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad        (void)
import           Control.Monad.Except (runExcept)
import           Data.Either          (fromRight)
import           Lib
import           Test.QuickCheck      (Arbitrary (arbitrary), Property,
                                       arbitrarySizedBoundedIntegral, choose,
                                       conjoin, disjoin, elements, quickCheck,
                                       sized, suchThat, (.&&.))
import           Test.QuickCheck.All  (verboseCheckAll)
import Test.QuickCheck (quickCheckAll)
import Text.Parsec (parse)

instance Arbitrary a => Arbitrary (Term a) where
    arbitrary = choose @Int (0, 10) >>= recurringGen
        where recurringGen 0 = Fix . LitF <$> arbitrary
              recurringGen n = do
                  n1 <- arbitrary
                  t1 <- recurringGen (n - 1)
                  t2 <- recurringGen (n - 1)
                  elements $ Fix <$> [LitF n1, AddF t1 t2, SubF t1 t2, MulF t1 t2, DivF t1 t2, NegF t1]

data EvalTest = LitTest Double
              | AddTest (Term Double) (Term Double)
              | SubTest (Term Double) (Term Double)
              | MulTest (Term Double) (Term Double)
              | DivTest (Term Double) (Term Double)
              | NegTest (Term Double)
              deriving Show

instance Arbitrary EvalTest where
    arbitrary = do
        n <- arbitrary
        t1 <- arbitrary
        t2 <- arbitrary
        elements [LitTest n, AddTest t1 t2, SubTest t1 t2, MulTest t1 t2, DivTest t1 t2, NegTest t1]

evalNormal :: Term Double -> Double
evalNormal = fromRight undefined . runExcept . eval

evalCatchError :: Term Double -> (Double -> Bool) -> Bool
evalCatchError t k =
    case runExcept (eval t) of
        Left "Divide by zero" -> True
        Left x                -> error x
        Right n               -> k n

evalWrapper :: Term Double -> Term Double -> (Double -> Double -> Double) -> (Term Double -> Term Double -> Term Double) -> Bool
evalWrapper a b op term =
    evalCatchError (term a b) $ \t ->
    evalCatchError a $ \a ->
    evalCatchError b $ \b ->
     op a b == t

prop_evalCorrect :: EvalTest -> Bool
prop_evalCorrect (LitTest x)   = x == evalNormal (litf x)
prop_evalCorrect (AddTest a b) = evalWrapper a b (+) (+)
prop_evalCorrect (SubTest a b) = evalWrapper a b (-) (-)
prop_evalCorrect (MulTest a b) = evalWrapper a b (*) (*)
prop_evalCorrect (DivTest a b) = evalWrapper a b (/) divf
prop_evalCorrect (NegTest x)   = evalCatchError (negf x) $ \t -> evalCatchError x $ \x -> - x == t

prop_parseLitCorrect :: Double -> Bool
prop_parseLitCorrect d = either (const False) ((== d) . evalNormal) (parse parseLit "" (show d))

return []
main :: IO ()
main = void $quickCheckAll

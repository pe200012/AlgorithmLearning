{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where
import           Control.Monad       (void)
import           Control.Monad.Fix   (MonadFix (mfix), fix)
import           Coordinate          (DistanceData2D (..), Point,
                                      calculate2DCoordinates)
import           Data.Function       ((&))
import           Linear              (V2 (V2), crossZ, distance)
import           Test.QuickCheck     (Arbitrary (arbitrary), quickCheck,
                                      suchThat, withMaxSuccess)
import           Test.QuickCheck     (quickCheckAll)
import           Test.QuickCheck.All (verboseCheckAll)
import           Debug.Trace         (trace)

data DistanceData2DTest = DistanceData2DTest DistanceData2D Point deriving Show

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary DistanceData2DTest where
    arbitrary = do
        target <- arbitrary
        a <- suchThat arbitrary ((/= target))
        b <- suchThat arbitrary (and . flip map [(/= target), (/= a)] . (&) )
        c <- suchThat arbitrary (and . flip map [(/= target), (/= a), (/= b), ((/= 0) . crossZ (a - b) . (a -))] . (&) )
        return $ DistanceData2DTest (DistanceData2D (a, distance a target) (b, distance b target) (c, distance c target)) target

prop_calcuate2DCoordinatesCorrect :: DistanceData2DTest -> Bool
prop_calcuate2DCoordinatesCorrect (DistanceData2DTest distanceData target) = 
    let result = calculate2DCoordinates distanceData
    in trace (show result) distance result target < 0.1


return []
runTest = quickCheck (withMaxSuccess 10000 prop_calcuate2DCoordinatesCorrect)

main :: IO ()
main = void runTest

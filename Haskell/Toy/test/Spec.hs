{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck (Arbitrary(..), quickCheck, quickCheckAll)
import Engine.CollisionDetection (pointsTo, InternalPoint, getEdges)
import Engine.QuadTree ()
import Linear.V2 (V2(..))
import           Linear.Metric (distance, norm, project)

instance Arbitrary (V2 Double) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

prop_PointsToMeansVectorSubtraction :: InternalPoint -> InternalPoint -> Bool
prop_PointsToMeansVectorSubtraction a b = (a `pointsTo` b) == b - a

prop_SumOfAllEdgesEqualsZero :: [InternalPoint] -> Bool
prop_SumOfAllEdgesEqualsZero ps = case getEdges ps of
                                    [] -> True
                                    es -> (norm . sum $ uncurry pointsTo <$> es) <= 0.0001

main :: IO ()
main = do
    quickCheck prop_PointsToMeansVectorSubtraction
    quickCheck prop_SumOfAllEdgesEqualsZero

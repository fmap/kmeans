module Algorithms.Lloyd.SequentialSpec (spec, y) where

import Prelude hiding (take, zipWith, length)
import Algorithms.Lloyd.Sequential (Point(..), Cluster(..), step)
import Control.Applicative ((<$>), (<*>))
import Data.Metric (Euclidean(..))
import Data.Random.Normal (mkNormals)
import Data.Vector (Vector, generate, fromList, take, zipWith, length)
import Test.Hspec (Spec(..), describe, it)
import Test.QuickCheck (property, Arbitrary(..), choose)

data Structure = Structure
  { points   :: Vector Point
  , clusters :: Vector Cluster
  } deriving (Show)

instance Arbitrary Structure where
  arbitrary = do
    normals <- mkNormals <$> arbitrary
    (a1, k) <- (,) <$> choose (1,200) <*> choose (1,200)
    let points   = generate a1 $ \n -> Point $ fromList [normals !! n, normals !! (n*2)]
        clusters = zipWith Cluster (fromList [0..k-1]) (take k points)
    return $ Structure points clusters

spec :: Spec
spec = do
  describe "step" . it "Cluster length should be invariant upon repeated application." . property $ \(Structure points clusters) -> do
    let fixedPoint = y (step Euclidean `flip` points) clusters
    length clusters == length fixedPoint

y :: Eq a => (a -> a) -> a -> a
y f x = if x == f x then x else y f (f x)

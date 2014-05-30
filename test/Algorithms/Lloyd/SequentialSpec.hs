module Algorithms.Lloyd.SequentialSpec (spec, y) where

import Prelude hiding (take, zipWith, length, head)
import Algorithms.Lloyd.Sequential (Point(..), Cluster(..), step)
import Data.Metric (Euclidean(..))
import Data.Random.Normal (mkNormals)
import Data.Vector (Vector, generate, fromList, take, zipWith, length, head)
import Test.Hspec (Spec(..), describe, it, shouldBe)

points :: Vector Point
points = generate 2000 $ \n -> Point $ fromList [normals !! n, normals !! (n*2)]
  where normals = mkNormals 0x29a

clusters :: Vector Cluster
clusters = zipWith Cluster (fromList [0..2]) (take 3 points)

spec :: Spec
spec = 
  describe "step" . it "Cluster length should be invariant upon repeated application." $ do
    let fixedPoint = y (step Euclidean `flip` points) clusters
    length clusters `shouldBe` length fixedPoint

y :: Eq a => (a -> a) -> a -> a
y f x = if x == f x then x else y f (f x)

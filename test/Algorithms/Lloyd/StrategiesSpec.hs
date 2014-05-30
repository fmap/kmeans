module Algorithms.Lloyd.StrategiesSpec (spec) where

import Prelude hiding (take, zipWith, length, head)
import Algorithms.Lloyd.Strategies (Point(..), Cluster(..), step)
import Algorithms.Lloyd.SequentialSpec (y)
import Data.Metric (Euclidean(..))
import Data.Random.Normal (mkNormals)
import Data.Vector (Vector, generate, fromList, take, zipWith, length, head)
import Data.Vector.Split (chunksOf)
import Test.Hspec (Spec(..), describe, it, shouldBe)

points :: Vector (Vector Point)
points = chunksOf 30 . generate 2000 $ \n -> Point $ fromList [normals !! n, normals !! (n*2)]
  where normals = mkNormals 0x29a

clusters :: Vector Cluster
clusters = zipWith Cluster (fromList [0..2]) (take 3 $ head points)

spec :: Spec
spec = 
  describe "step" . it "Cluster length should be invariant upon repeated application." $ do
    let fixedPoint = y (step Euclidean `flip` points) clusters
    length clusters `shouldBe` length fixedPoint

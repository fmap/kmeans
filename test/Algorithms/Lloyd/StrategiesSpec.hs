module Algorithms.Lloyd.StrategiesSpec (spec) where

import Prelude hiding (take, zipWith, length, head)
import Algorithms.Lloyd.Strategies (Point(..), Cluster(..), step)
import Algorithms.Lloyd.SequentialSpec (y)
import Control.Applicative ((<$>),(<*>))
import Data.Metric (Euclidean(..))
import Data.Random.Normal (mkNormals)
import Data.Vector (Vector, generate, fromList, take, zipWith, length, head)
import Data.Vector.Split (chunksOf)
import Test.Hspec (Spec(..), describe, it)
import Test.QuickCheck (property, Arbitrary(..), choose)

data Structure = Structure
  { points   :: Vector (Vector Point)
  , clusters :: Vector Cluster
  } deriving (Show)

instance Arbitrary Structure where
  arbitrary = do
    normals   <- mkNormals <$> arbitrary
    (a0,a1,k) <- (,,) <$> choose (1,200) <*> choose (1,200) <*> choose (1,200)
    let points   = chunksOf a0 . generate a1 $ \n -> Point $ fromList [normals !! n, normals !! (n*2)]
        clusters = zipWith Cluster (fromList [0..k-1]) (take k $ head points)
    return $ Structure points clusters

spec :: Spec
spec = do
  describe "step" . it "Cluster length should be invariant upon repeated application." . property $ \(Structure points clusters) -> do
    let fixedPoint = y (step Euclidean `flip` points) clusters
    length clusters == length fixedPoint

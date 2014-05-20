> {-# LANGUAGE ViewPatterns #-}

A sequential implementation of Lloyd's algorithm for k-means clustering,
adapted from Marlow's _Parallel and Concurrent Programming in Haskell_:

> module Algorithms.Lloyd.Sequential where
> 
> import Prelude hiding (zipWith, map, foldr1, replicate)
> import Control.Monad (guard, forM_)
> import Data.Function (on)
> import Data.Functor.Extras ((..:),(...:))
> import Data.List (minimumBy)
> import Data.Metric (Metric(..))
> import Data.Semigroup (Semigroup(..))
> import Data.Vector (Vector(..), toList, create, zipWith, map, foldr1, empty, replicate)
> import qualified Data.Vector.Mutable as MV (replicate, read, write)
 
A data point is comprised of an n-dimensional real vector, and an
associated value:

> data Point a = Point 
>   { point :: Vector Double
>   , value :: a
>   } 

Two data-points are defined as equivalent if they've a point vector in
common:

> instance Eq (Point a) where
>   (==) = (==) `on` point

To define distance between data-points, we admit arbitrary metric spaces
over real vectors; **However:** it should be noted that convergence is
only guaranteed in the case when the mean of the metric optimises the
same criterion as minimum-distance assignment (`assign` below.) For this
reason, Euclidean-distance is popularly used for mean assignment.

> useMetric :: Metric a => (Vector Double -> a) -> Vector Double -> Vector Double -> Double
> useMetric = on distance

Clusters are represented by an identifier and a centroid:

> data Cluster = Cluster
>   { identifier :: Int
>   , centroid   :: Vector Double
>   } deriving (Show)

Clusters are treated as equivalent if they share a centroid:

> instance Eq Cluster where
>   (==) = (==) `on` centroid

`PointSum` is a point sum; this is an intermediate type used to contain the sum
of points in a set when constructing new clusters. It consists of a count of
included points, as well as the vector sum of the constituent points:

> data PointSum = PointSum Int (Vector Double) deriving (Show)

Two `PointSum`s can be combined by summing corresponding values:

> instance Semigroup PointSum where
>   PointSum c0 p0 <> PointSum c1 p1 = PointSum (c0+c1) (zipWith (+) p0 p1)

Without dependent types, we can't define a valid Monoid instance for PointSum,
as the identity varies with the dimensions of the point vector. But we can
construct one at runtime:

> emptyPointSum :: Int -> PointSum
> emptyPointSum length = PointSum 0 $ replicate length 0

A point sum is constructed by incrementally adding points likeso:

> addPoint :: PointSum -> Point a -> PointSum
> addPoint sum = (sum <>) . PointSum 1 . point

A point sum can be turned into a cluster by computing its centroid, the average
of all points associated with the cluster:

> toCluster :: Int -> PointSum -> Cluster
> toCluster cid (PointSum count vector) = Cluster
>   { identifier = cid
>   , centroid   = map (//count) vector
>   }
>
> (//) :: Double -> Int -> Double
> x // y = x / fromIntegral y
           
After each iteration, points are associated with the cluster having the nearest
centroid:

> closestCluster :: Metric a => (Vector Double -> a) -> [Cluster] -> Point b -> Cluster
> closestCluster (useMetric -> d) clusters (point->point) = fst . minimumBy (compare `on` snd) $ do
>   cluster <- clusters
>   return (cluster, point `d` centroid cluster)
>
> assign :: Metric a => (Vector Double -> a) -> [Cluster] -> [Point b] -> Vector PointSum
> assign metric clusters points = let nc = length clusters in create $ do
>   vector <- MV.replicate nc $ emptyPointSum nc
>   points `forM_` \point -> do
>     let cluster  = closestCluster metric clusters point 
>         position = identifier cluster
>     sum <- MV.read vector position
>     MV.write vector position $! addPoint sum point
>   return vector
>
> makeNewClusters :: Vector PointSum -> [Cluster]
> makeNewClusters vector = do
>   (pointSum@(PointSum count _), index) <- zip (toList vector) [0..]
>   guard $ count > 0 -- We don't want an empty PointSum: amongst other 
>                     -- things, this'd lead to division by zero when 
>                     -- computing the centroid.
>   return $ toCluster index pointSum
>
> step :: Metric a => (Vector Double -> a) -> [Cluster] -> [Point b] -> [Cluster]
> step = makeNewClusters ...: assign

The algorithm consists of iteratively finding the centroid of each existing
cluster, then reallocating points according to which centroid is closest, until
convergence. As the algorithm isn't guaranteed to converge, we cut execution if
convergence hasn't been observed after eighty iterations:

> kmeans :: Metric a => (Vector Double -> a) -> [Point b] -> [Cluster] -> [Cluster]
> kmeans = flip kmeans' 0 
>
> kmeans' :: Metric a => (Vector Double -> a) -> Int -> [Point b] -> [Cluster] -> [Cluster]
> kmeans' metric iterations points clusters 
>   | iterations >= expectDivergent = clusters
>   | clusters' == clusters         = clusters 
>   | otherwise                     = kmeans' metric (succ iterations) points clusters'
>   where clusters' = step metric clusters points
>
> expectDivergent :: Int
> expectDivergent = 80

A note on initialisation: typically clusters are randomly assigned to data
points prior to the first update step.

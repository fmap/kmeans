A sequential implementation of Lloyd's algorithm for k-means clustering,
adapted from Marlow's _Parallel and Concurrent Programming in Haskell_:

> module Algorithms.Lloyd.Sequential where
> 
> import Data.Monoid (Monoid(..))
> import Data.Function (on)
> import Data.List (minimumBy)
> import Data.Vector (Vector(..), toList, create)
> import Control.Monad (guard, forM_)
> import qualified Data.Vector.Mutable as MV (replicate, read, write)
 
A data point is represented by two real coordinates; despite that Lloyd's
algorithm is generalisable to an arbitrary number of dimensions:

> data Point = Point Double Double deriving (Eq, Show)
 
The `Metric` typeclass, as defined here, is intended to contain types that
admit metrics (i.e. are metric spaces.) Instances can be defined in terms of 
`distance` or the infix `<->`:

> class Metric a where
>   distance :: a -> a -> Double
>   distance = (<->)
>   (<->) :: a -> a -> Double
>   (<->) = distance

We define distance between data-points as the sum of squares of differences of
corresponding coordinates. The actual (Euclidean) distance is given by the
square root of this value, but as we're only interested in comparing distances,
we omit that function for efficiency:

> instance Metric Point where
>   Point x1 y1 <-> Point x2 y2 = (x2-x1)**2 + (y2-y1)**2
 
`Point`s may be combined by summing their coordinates, in this case the origin
is identity:

> instance Monoid Point where
>   Point x0 y0 `mappend` Point x1 y1 = Point (x0+x1) (y0+y1)
>   mempty = Point 0 0 

Clusters are represented by an identifier and a centroid:

> data Cluster = Cluster
>   { identifier :: Int
>   , centroid   :: Point
>   } deriving (Show)

Clusters are treated as equivalent if they share a centroid:

> instance Eq Cluster where
>   (Cluster _ c) == (Cluster _ d) = c == d

`PointSum` is a point sum; this is an intermediate type used to contain the sum
of points in a set when constructing new clusters. It consists of a count of
included points, as well as the sum of their x and y coordinates:

> data PointSum = PointSum Int Double Double deriving (Show)

Two `PointSum`s can be combined by summing corresponding values; the `PointSum`
representing the sum of no points is identity:

> instance Monoid PointSum where
>   PointSum c0 x0 y0 `mappend` PointSum c1 x1 y1 = PointSum (c0+c1) (x0+x1) (y0+y1)
>   mempty = PointSum 0 0 0 

A point sum is constructed by incrementally adding points likeso:

> addPoint :: PointSum -> Point -> PointSum
> addPoint (PointSum cn xs ys) (Point x y) = PointSum (succ cn) (xs + x) (ys + y)

A point sum can be turned into a cluster by computing its centroid, the average
of all points associated with the cluster:

> toCluster :: Int -> PointSum -> Cluster
> toCluster cid (PointSum count xs ys) = Cluster
>   { identifier = cid
>   , centroid   = Point (xs // count) (ys // count)
>   }
>
> (//) :: Double -> Int -> Double
> x // y = x / fromIntegral y
 
After each iteration, points are associated with the cluster having the nearest
centroid:

> closestCluster :: [Cluster] -> Point -> Cluster
> closestCluster clusters point = fst . minimumBy (compare `on` snd) $ do
>   cluster <- clusters
>   return (cluster, point <-> centroid cluster)
>
> assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
> assign nclusters clusters points = create $ do
>   vector <- MV.replicate nclusters mempty
>   points `forM_` \point -> do
>     let cluster  = closestCluster clusters point 
>         position = identifier cluster
>     pointsum <- MV.read vector position
>     MV.write vector position $! addPoint pointsum point
>   return vector

> makeNewClusters :: Vector PointSum -> [Cluster]
> makeNewClusters vector = do
>   (pointSum@(PointSum count _ _), index) <- zip (toList vector) [0..]
>   guard $ count > 0 -- We don't want an empty PointSum: amongst other 
>                     -- things, this'd lead to division by zero when 
>                     -- computing the centroid.
>   return $ toCluster index pointSum
>
> step :: Int -> [Cluster] -> [Point] -> [Cluster]
> step = makeNewClusters ...: assign
>
> (...:) :: (Functor f, Functor g, Functor h) => (a -> b) -> f(g(h a)) -> f(g(h b))
> (...:) = fmap . fmap . fmap

The algorithm consists of iteratively finding the centroid of each existing
cluster, then reallocating points according to which centroid is closest, until
convergence. As the algorithm isn't guaranteed to converge, we cut execution if
convergence hasn't been observed after eighty iterations:

> kmeans :: Int -> [Point] -> [Cluster] -> [Cluster]
> kmeans = kmeans' 0
>
> kmeans' :: Int -> Int -> [Point] -> [Cluster] -> [Cluster]
> kmeans' iterations nclusters points clusters 
>   | iterations >= expectDivergent = clusters
>   | clusters' == clusters         = clusters 
>   | otherwise                     = kmeans' (succ iterations) nclusters points clusters'
>   where clusters' = step nclusters clusters points
>
>
> expectDivergent :: Int
> expectDivergent = 80

A note on initialisation: typically clusters are randomly assigned to data
points prior to the first update step.

A sequential implementation of Lloyd's algorithm for k-means clustering,
adapted from Marlow's _Parallel and Concurrent Programming in Haskell_:

> module Algorithms.Lloyd.Sequential where
> 
> import Prelude hiding (zipWith, map, foldr1, replicate)
> import Control.Monad (guard, forM_)
> import Data.Function (on)
> import Data.List (minimumBy)
> import Data.Semigroup (Semigroup(..))
> import Data.Vector (Vector(..), toList, create, zipWith, map, foldr1, empty, replicate)
> import qualified Data.Vector.Mutable as MV (replicate, read, write)
 
A data point is represented by an n-dimensional real vector:

> data Point = Point (Vector Double) deriving (Eq, Show)

Point isn't parametrised, so we can't define a functor (or any functor-family)
instances. Instead, we define a specialised map, `pmap`:

> pmap :: (Double -> Double) -> Point -> Point
> pmap f (Point vector) = Point $ map f vector
 
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
>   Point v0 <-> Point v1 = foldr1 (+) . map (**2) $ zipWith (-) v0 v1
 
`Point`s may be combined using simple vector addition:

> instance Semigroup Point where
>   Point v0 <> Point v1 = Point $ zipWith (+) v0 v1

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
included points, as well as the vector sum of the constituent points:

> data PointSum = PointSum Int Point deriving (Show)

Two `PointSum`s can be combined by summing corresponding values:

> instance Semigroup PointSum where
>   PointSum c0 p0 <> PointSum c1 p1 = PointSum (c0+c1) (p0<>p1)

Without dependent types, we can't define a valid Monoid instance for PointSum,
as the identity varies with the dimensions of the point vector. But we can
construct one at runtime:

> emptyPointSum :: Int -> PointSum
> emptyPointSum length = PointSum 0 . Point $ replicate length 0

A point sum is constructed by incrementally adding points likeso:

> addPoint :: PointSum -> Point -> PointSum
> addPoint sum point = sum <> pure point
>   where pure = PointSum 1 

A point sum can be turned into a cluster by computing its centroid, the average
of all points associated with the cluster:

> toCluster :: Int -> PointSum -> Cluster
> toCluster cid (PointSum count point) = Cluster
>   { identifier = cid
>   , centroid   = pmap (//count) point
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
> assign :: [Cluster] -> [Point] -> Vector PointSum
> assign clusters points = let nc = length clusters in create $ do
>   vector <- MV.replicate nc $ emptyPointSum nc
>   points `forM_` \point -> do
>     let cluster  = closestCluster clusters point 
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
> step :: [Cluster] -> [Point] -> [Cluster]
> step = makeNewClusters ..: assign
>
> (..:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
> (..:) = fmap . fmap
> 
> infixr 8 ..:

The algorithm consists of iteratively finding the centroid of each existing
cluster, then reallocating points according to which centroid is closest, until
convergence. As the algorithm isn't guaranteed to converge, we cut execution if
convergence hasn't been observed after eighty iterations:

> kmeans :: [Point] -> [Cluster] -> [Cluster]
> kmeans = kmeans' 0
>
> kmeans' :: Int -> [Point] -> [Cluster] -> [Cluster]
> kmeans' iterations points clusters 
>   | iterations >= expectDivergent = clusters
>   | clusters' == clusters         = clusters 
>   | otherwise                     = kmeans' (succ iterations) points clusters'
>   where clusters' = step clusters points
>
> expectDivergent :: Int
> expectDivergent = 80

A note on initialisation: typically clusters are randomly assigned to data
points prior to the first update step.

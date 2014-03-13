A parallel implementation of Lloyd's algorithm for k-means clustering,
adapted from Marlow's _Parallel and Concurrent Programming in Haskell_.
Here we use Evaluation Strategies to parallelise the assignment of
points to clusters:

> module Algorithms.Lloyd.Strategies where
>
> import Prelude hiding (zipWith)
> import Control.Parallel.Strategies (Strategy(..), parTraversable, using, rseq)
> import Data.List.Split (chunksOf)
> import Data.Semigroup (Semigroup(..))
> import Data.Vector (Vector(..), zipWith)
> import Algorithms.Lloyd.Sequential (Cluster(..), Point(..), PointSum(..), makeNewClusters, assign, (..:), expectDivergent)

We can combine two vectors of some same type $t$ provided we know how to
combine two $t$s:

> instance Semigroup a => Semigroup (Vector a) where
>   (<>) = zipWith (<>)

Step is modified to, given a partitioned list of points, perform
classification in parallel:

> step :: [Cluster] -> [[Point]] -> [Cluster]
> step = makeNewClusters . foldr1 (<>) . with (parTraversable rseq) ..: map . assign 
>
> with :: Strategy a -> a -> a
> with = flip using

This version of k-means takes an additional arguments -- the number of
partitions the set of points'll be divided into. This needn't equal the
number of processors: if there are more spark than cores, the runtime
can be trusted to schedule unallocated sparks so soon as a core becomes
available. That said: if there are too many work items, the overhead of
recombination may exceed the speed-up provided by parallellism; if there
are too few items, and those items vary in cost, some of our cores may
be unused for part of the computation.

> kmeans :: Int -> [Point] -> [Cluster] -> [Cluster]
> kmeans = kmeans' 0 ..: chunksOf
>
> kmeans' :: Int -> [[Point]] -> [Cluster] -> [Cluster]
> kmeans' iterations points clusters 
>   | iterations >= expectDivergent = clusters
>   | clusters' == clusters         = clusters 
>   | otherwise                     = kmeans' (succ iterations) points clusters'
>   where clusters' = step clusters points

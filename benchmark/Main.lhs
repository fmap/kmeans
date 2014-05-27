We aim to benchmark each implementation of Lloyd's algorithm:

> import Prelude hiding (take, zipWith)
> import Algorithms.Lloyd.Sequential (Point(..), Cluster(..), ExpectDivergent(..))
> import Algorithms.Lloyd.Strategies (Partitions(..))
> import qualified Algorithms.Lloyd.Sequential as Sequential (kmeans)
> import qualified Algorithms.Lloyd.Strategies as Strategies (kmeans)
> import Data.Metric (Metric(..), Euclidean(..))
> import Data.Random.Normal (mkNormals)
> import Data.Vector (Vector, generate, fromList, take, zipWith)
> import Control.Monad (forM)
> import Control.DeepSeq (NFData(..))
> import Criterion.Main (defaultMain, bench, nf)

We draw 2e3 normally distributed 2D points:

> points :: Vector Point
> points = generate 2000 $ \n -> Point $ fromList [normals !! n, normals !! (n*2)]
>   where normals = mkNormals 0x29a

Three of which form the centroids of our initial clusters:
 
> clusters :: Vector Cluster
> clusters = zipWith Cluster (fromList [0..2]) (take 3 points)

To correctly benchmark the result of a pure function, we need to be able to
evaluate it to normal form:

> instance NFData Cluster where
>   rnf (Cluster i c) = rnf i `seq` rnf c
>
> instance NFData Point where
>   rnf (Point v) = rnf v
 
Together the subject of our benchmarks:
 
> main :: IO ()
> main = defaultMain
>   [ bench "Sequential" $ nf (Sequential.kmeans expectDivergent Euclidean points) clusters
>   , bench "Strategies" $ nf (Strategies.kmeans expectDivergent Euclidean partitions points) clusters
>   ] where (expectDivergent, partitions) = (ExpectDivergent 80, Partitions 64)

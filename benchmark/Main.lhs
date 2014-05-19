We aim to benchmark each implementation of Lloyd's algorithm:

> import Prelude hiding (take, zipWith)
> import qualified Prelude (take)
> import Algorithms.Lloyd.Sequential (Point(..), Cluster(..))
> import Data.Monoid (Monoid(mconcat))
> import qualified Algorithms.Lloyd.Sequential as Sequential (kmeans)
> import qualified Algorithms.Lloyd.Strategies as Strategies (kmeans)
> import Data.Metric (Metric(..), Euclidean(..))
> import Data.Random.Normal (mkNormals)
> import Data.Vector (Vector, fromList, take, zipWith, generate)
> import Control.Monad (forM)
> import Control.DeepSeq (NFData(..))
> import Criterion.Main (defaultMain, bench, nf)

We draw 2e3 normally distributed 2D points:

> points :: Vector Point
> points = generate 2000 $ \n -> Point $ fromList [normals !! n, normals !! (n*2)]
>   where normals = Prelude.take 4000 $ mkNormals 0x29a

Three of which form the centroids of our initial clusters:
 
> clusters :: Vector Cluster
> clusters = zipWith Cluster (fromList [0..]) (take 3 points)

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
>   [ bench "Sequential" $ nf (Sequential.kmeans Euclidean points) clusters
>   , bench "Strategies" $ nf (Strategies.kmeans Euclidean 64 points) clusters
>   ] 

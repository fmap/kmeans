We aim to benchmark each implementation of Lloyd's algorithm:

> import Algorithms.Lloyd.Sequential (Point(..), Cluster(..))
> import qualified Algorithms.Lloyd.Sequential as Sequential (kmeans)
> import qualified Algorithms.Lloyd.Strategies as Strategies (kmeans)
> import Data.Metric (Metric(..), Euclidean(..))
> import Data.Random.Normal (mkNormals)
> import Data.Vector (fromList)
> import Control.Monad (forM)
> import Control.DeepSeq (NFData(..))
> import Criterion.Main (defaultMain, bench, nf)

We draw 2e3 normally distributed 2D points:

> points :: [Point Int]
> points = [Point (vector n) n | n <- [1..2000-1]]
>   where vector n = fromList [normals!!n, normals!!n*2]
>         normals  = mkNormals 0x29a

Three of which form the centroids of our initial clusters:
 
> clusters :: [Cluster]
> clusters = take 3 . map (uncurry Cluster) $ zip [0..] (map point points)

To correctly benchmark the result of a pure function, we need to be able to
evaluate it to normal form:

> instance NFData Cluster where
>   rnf (Cluster i c) = rnf i `seq` rnf c
>
> instance NFData a => NFData (Point a) where
>   rnf (Point a b) = rnf a `seq` rnf b
 
Together the subject of our benchmarks:
 
> main :: IO ()
> main = defaultMain
>   [ bench "Sequential" $ nf (Sequential.kmeans 80 Euclidean points) clusters
>   , bench "Strategies" $ nf (Strategies.kmeans 80 Euclidean 64 points) clusters
>   ] 

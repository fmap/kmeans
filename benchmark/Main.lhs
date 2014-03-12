We aim to benchmark each implementation of Lloyd's algorithm:

> import Algorithms.Lloyd.Sequential (Point(..), Cluster(..))
> import qualified Algorithms.Lloyd.Sequential as Sequential (kmeans)
> import System.Random (split, mkStdGen)
> import Data.Random.Normal (normals)
> import Control.DeepSeq (NFData(..))
> import Criterion.Main (defaultMain, bench, nf)

We draw 2e3 normally distributed points:

> points :: [Point]
> points = take 2000 . map (uncurry Point) $ zipWith (,) (normals x) (normals y)
>   where (x, y) = split $ mkStdGen 0x29a
 
Three of which form the centroids of our initial clusters:
 
> clusters :: [Cluster]
> clusters = map (uncurry Cluster) $ zip [0..] (take 3 points)

To correctly benchmark the result of a pure function, we need to be able to
evaluate it to normal form:

> instance NFData Cluster where
>   rnf (Cluster i c) = rnf i `seq` rnf c
>
> instance NFData Point where
>   rnf (Point x y) = rnf x `seq` rnf y
 
Together the subject of our benchmarks:
 
> main :: IO ()
> main = defaultMain
>   [ bench "Sequential" $ nf (Sequential.kmeans nc points) clusters
>   ] where nc = length clusters

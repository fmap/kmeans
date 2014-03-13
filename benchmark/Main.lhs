We aim to benchmark each implementation of Lloyd's algorithm:

> import Algorithms.Lloyd.Sequential (Point(..), Cluster(..))
> import qualified Algorithms.Lloyd.Sequential as Sequential (kmeans)
> import qualified Algorithms.Lloyd.Strategies as Strategies (kmeans)
> import Data.Random.Normal (mkNormals)
> import Data.Vector (fromList)
> import Control.Monad (forM)
> import Control.DeepSeq (NFData(..))
> import Criterion.Main (defaultMain, bench, nf)

We draw 2e3 normally distributed 2D points:

> points :: [Point]
> points = concat $ [0..2000-1] `forM` \n -> do
>   return . Point . fromList $ [normals!!n, normals!!(n*2)]
>   where normals = take 4000 $ mkNormals 0x29a

Three of which form the centroids of our initial clusters:
 
> clusters :: [Cluster]
> clusters = map (uncurry Cluster) $ zip [0..] (take 3 points)

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
>   [ bench "Sequential" $ nf (Sequential.kmeans points) clusters
>   , bench "Strategies" $ nf (Strategies.kmeans 64 points) clusters
>   ] 

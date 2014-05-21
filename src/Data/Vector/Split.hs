module Data.Vector.Split (chunksOf) where

import Prelude hiding (take, drop, null)
import Data.Vector (Vector(..), cons, take, drop, empty, null)

chunksOf :: Int -> Vector a -> Vector (Vector a)
chunksOf n vector 
  | null vector = empty
  | otherwise   = take n vector `cons` chunksOf n (drop n vector)

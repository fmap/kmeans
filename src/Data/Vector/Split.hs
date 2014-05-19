module Data.Vector.Split (chunksOf) where

import Prelude hiding (drop, null)
import Data.Vector (Vector(..), cons, drop, empty, null)

chunksOf :: Int -> Vector a -> Vector (Vector a)
chunksOf n vector 
  | null vector = empty
  | otherwise   = vector `cons` chunksOf n (drop n vector)

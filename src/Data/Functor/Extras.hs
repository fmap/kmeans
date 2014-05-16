module Data.Functor.Extras (
  (..:),
  (...:)
) where

(..:) :: (Functor f, Functor g) => (a -> b) -> f(g(a)) -> f(g(b))
(..:) = fmap fmap fmap
infixl 4 ..:

(...:) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(...:) = fmap . fmap . fmap

infixr 4 ...:

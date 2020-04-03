module Chapter18.Monad(
ap
)where

import Chapter17.Applicative
import Control.Monad(join)

ap :: Monad f => f (a -> b) -> f a -> f b
ap af ax = af >>= (\f -> ax >>= (\x -> (return . f) x))

instance Monad ListA where
  return = pure
  xs >>= f = concatList $ fmap f xs

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

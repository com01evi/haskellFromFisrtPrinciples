module Chapter26.MonadTransformers(
)where

import Control.Monad.Identity

innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

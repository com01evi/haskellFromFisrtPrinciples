module Chapter16.Functor
    (
    f
    ) where

f = let either = Right "Hey" in head <$> either

module Chapter15.Semigroup
    (
    NonEmpty(NonEmpty, (:|))
    ) where

data NonEmpty a = NonEmpty a | a :| NonEmpty a deriving (Eq, Ord, Show)

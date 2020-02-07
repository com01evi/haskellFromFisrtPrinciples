module Chapter1
    (
    test
    ) where

test :: (Eq b) => (a -> b) -> (a -> b) -> (a -> Bool)
test f g = (==) <$> f <*> g

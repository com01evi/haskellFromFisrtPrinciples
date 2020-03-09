module Chapter14.Exercise
    (
    half,
    halfIndentity,
    listOrdered,
    plusAssociative,
    multAssociative
    ) where

half :: (Fractional a) => a -> a
half x = x / 2

halfIndentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_,False) = status
        go x (Nothing, t) = (Just x, t)
        go x (Just y, t) = (Just x, y>=x)

plusAssociative x y z = x + (y + z) == (x + y) + z

multAssociative x y z = x * (y * z) == (x * y) * z



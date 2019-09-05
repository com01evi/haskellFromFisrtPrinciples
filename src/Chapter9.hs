module Chapter9
    ( 
    maybeTail,
    maybeHead
    ) where

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:[]) = Nothing
maybeTail (_:xs) = Just xs

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

module Chapter8
    (
    incTimes,
    appryTimes
    ) where

incTimes :: Int -> Int -> Int
incTimes 0 n = n
incTimes times n = 1 + incTimes (times -1) n

appryTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
appryTimes 0 f b = b 
appryTimes n f b = f (appryTimes (n-1) f b)

appryTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
appryTimes' 0 f b = b
appryTimes' n f b = f . appryTimes' (n-1) f $ b

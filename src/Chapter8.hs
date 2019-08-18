module Chapter8
    ( 
      fact,
      inc,
      three',
      applyTimes',
      f
    ) where

fact :: Int -> Int
fact n 
  | n <= 0 = 1
  | otherwise = n * fact (n-1)

inc :: Num a => a -> a
inc = (+1)

three' = (inc . inc . inc) 0

incTimes :: (Ord a, Num a) => a -> a -> a
incTimes times n
  | times <= 0 = n
  | otherwise = 1 + (incTimes (times - 1) n)

applyTimes :: (Ord a, Num a) => a -> (b -> b) -> b -> b
applyTimes n f b 
  | n <= 0 = b
  | otherwise = f (applyTimes (n-1) f b)

incTimes' :: (Ord a, Num a) => a -> a -> a
incTimes' n b = applyTimes n (+1) b

applyTimes' :: (Ord a, Num a) => a -> (b -> b) -> b -> b
applyTimes' n f b 
  | n <= 0 = b
  | otherwise = f . applyTimes (n-1) f $ b

f :: Bool -> Int
f True = error "Blah"
f False = 0

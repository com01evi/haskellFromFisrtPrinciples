module Chapter7
    (
     roundTrip 
    ) where


tensDigit :: Integral a =>  a -> a
tensDigit = snd . flip divMod 10 

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b = if b then x else y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b == True = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b,c)
g f (x,y) = (f x, y)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show 

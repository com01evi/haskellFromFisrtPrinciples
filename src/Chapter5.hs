module Chapter5
    (
    ) where

data X
data Y
data Z

xz :: X -> Z 
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X,Y) -> (Z,Z)
xform = (,) <$> (xz . fst) <*> (yz . snd)

munge :: (x -> y) -> (y -> (w,z)) -> x -> w 
munge = undefined

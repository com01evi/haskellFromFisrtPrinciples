module Chapter6
    ( TisAnInteger(..) 
    ) where


data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn x) == (TisAn y) = x == y

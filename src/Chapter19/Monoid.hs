module Chapter19.Monoid(
h
)where

import Data.Monoid

f x = const (Sum $ 1+x)
g x = const (Sum $ 2+x)
h = f <> g

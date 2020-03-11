module Chapter15.List
    (
      list
    , list2
    , List(Nil,Cons)
    , append
    , myProduct
    , myHead
    ) where

import Chapter11
import Data.Monoid

list = mappend [1..3] [4..10]

list2 = mconcat [[1..3],[4..20]]

append :: List a -> List a -> List a
append Nil list@(Cons x _) = list
append list@(Cons x _) Nil = list
append (Cons x list) list2 = Cons x (append list list2)

sumResult = mappend (Sum 1) (Sum 2)

myProduct :: [Int] -> Int
myProduct = getProduct . mconcat . fmap Product

fiftyfive = foldl mappend mempty ([1,10] :: [Sum Int])

myHead :: [a] -> Maybe a
myHead = getFirst . mconcat . fmap (First . Just)

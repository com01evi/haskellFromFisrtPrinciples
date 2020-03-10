module Chapter15.List
    (
      list
    , list2
    , List(Nil,Cons)
    , append
    ) where

import Chapter11

list = mappend  [1..3] [4..10]

list2 = mconcat [[1..3],[4..20]]

append :: List a -> List a -> List a
append Nil list@(Cons x _) = list
append list@(Cons x _) Nil = list
append (Cons x list) list2 = Cons x (append list list2)

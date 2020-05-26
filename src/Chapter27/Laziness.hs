{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Chapter27.Laziness(
answer,
tracef,
traceg,
lazyEval,
banging,
first',
second',
Foo(Foo),
blah,
oneEl,
hogex,
mainS
)where

import Data.List.Ordered
import Debug.Trace

hypo :: IO ()
hypo = do 
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"

list = [1,2,3,4,5]

ncombination :: Int -> [a] -> [[a]]
ncombination 0 list = [[]]
ncombination n list = (:) <$> list <*> ncombination (n-1) list

answer = filter ismySorted $ ncombination 3 list

ismySorted :: Ord a => [a] -> Bool
ismySorted (x:[]) = True
ismySorted (x:y:xs) = if x < y then ismySorted (y:xs) else False
                               
hypo' :: IO ()
hypo' = do 
  let x :: Int
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _ -> putStrLn "hello"

hypo'' :: IO ()
hypo'' = do 
  let x :: Int
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"

tracef x = (x ()) + (x ())

traceg = const (trace "hi" 2)

forever :: Monad m => m a -> m b
forever a = let a' = a >> a' in a'

lazyEval :: (a,b) -> String
lazyEval ~(x,y) = const "fire" x

banging :: Bool -> Int
banging !b = 1

data Foo = Foo Int !Int

first' :: Foo -> Int
first' (Foo x _) = x

second' :: Foo -> Int
second' (Foo _ y) = y

blah x = 1

data ListS a = Nil | Cons !a (ListS a) deriving(Eq, Show)

sTake :: Int -> ListS a -> ListS a
sTake n _
  | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x xs) = Cons x (sTake (n-1) xs)

twoEls = Cons undefined (Cons undefined Nil)
oneEl = sTake 0 twoEls

hogex :: Int -> Int
hogex x = x

x = undefined
y = "blah"
mainS = do 
    print (x `seq` snd (x, y))

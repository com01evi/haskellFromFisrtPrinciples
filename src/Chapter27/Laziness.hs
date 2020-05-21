{-# LANGUAGE OverloadedStrings #-}
module Chapter27.Laziness(
answer,
tracef,
traceg,
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

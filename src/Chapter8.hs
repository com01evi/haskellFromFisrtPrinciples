{-# LANGUAGE FlexibleContexts #-}

module Chapter8
    ( 
      fact,
      inc,
      three',
      applyTimes',
      f,
      fib,
      dividedBy,
      addNums,
      mul,
      mc91,
      digits,
      digitToWord,
      wordNumber
    ) where

import Data.List (intersperse)

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

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          | n > 0 && d < 0 = go (n - abs(d)) d (count-1)
          | n < 0 && d < 0 = go (abs(n) - abs(d)) d (count+1)
          |  abs(n) < abs(d) = (count,n)
          | otherwise = go (n - d ) d (count+1)

addNums :: (Eq a, Num a) => a -> a
addNums 1 = 1
addNums n = n + addNums (n-1)

mul :: (Integral a, Show a) => a -> a -> a
mul n 0 = 0
mul n i = n + mul n (i-1)

mc91 :: Int -> Int
mc91 n
  | n > 100 = n -10
  | otherwise = mc91(mc91(n+11))

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits = reverse . digits'

digits' :: Int -> [Int]
digits' n
  | (div n 10) == 0 = [mod n 10]
  | otherwise = (mod n 10) : digits' (div n 10) 

wordNumber :: Int -> String
wordNumber =  concat . intersperse "-" . map digitToWord . digits

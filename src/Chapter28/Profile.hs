module Chapter28.Profile(
  profileMain,
  memoryMain
)where

import Control.Monad

f :: IO ()
f = do
  print ([1..] !! 999999)
  putStrLn "f"

g :: IO ()
g = do
  print ([1..] !! 9999999)
  putStrLn "g"

profileMain :: IO ()
profileMain = do
  f 
  g

blah :: [Integer]
blah = [1..1000]

memoryMain :: IO ()
memoryMain = replicateM_ 10000 (print blah)

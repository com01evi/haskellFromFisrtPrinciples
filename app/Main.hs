module Main where

import Chapter2
import Chapter3
import Chapter4

main :: IO ()
main = do
  s <- getLine
  sayHello s
  helloWorld
  print $ changeMood Blah

module Main where

import Chapter2
import Chapter3

main :: IO ()
main = do
  s <- getLine
  sayHello s
  helloWorld

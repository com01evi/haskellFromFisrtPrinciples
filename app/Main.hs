module Main where

import Chapter2
import Chapter3
import Chapter4
import Chapter5
import Chapter6
import Chapter7
import Chapter8
import Chapter9
import Chapter10

main :: IO ()
main = do
  s <- getLine
  sayHello s
  helloWorld
  print $ changeMood Blah

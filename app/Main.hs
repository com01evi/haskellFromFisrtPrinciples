module Main where

import Chapter2

main :: IO ()
main = do 
  s <- getLine
  sayHello s

module Main where

import Chapter1

main :: IO ()
main = do 
  s <- getLine
  sayHello s

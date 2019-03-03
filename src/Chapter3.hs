module Chapter3
    (
      helloWorld
    , area
    , rvrs
    ) where

helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

area d = pi * r^2
  where r = d/2

rvrs :: String -> String
rvrs = unwords . (foldl (flip (:)) []) . words

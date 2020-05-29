module Chapter28.Profile(
profileMain
)where

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

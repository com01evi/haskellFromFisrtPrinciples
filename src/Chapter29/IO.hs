module Chapter29.IO(
  traceMain
)where

import Debug.Trace

blah :: IO String
blah = return "blah"

blah' :: IO String
blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

traceMain :: IO ()
traceMain = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w

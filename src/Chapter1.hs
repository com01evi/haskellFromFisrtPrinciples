module Chapter1
    ( sayHello
    ) where

sayHello :: String -> IO ()
sayHello s = print $ "Hello, " ++ s

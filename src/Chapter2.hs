module Chapter2
    ( sayHello
    , triple
    , square
    , square'
    , multPi
    ) where

sayHello :: String -> IO ()
sayHello s = print $ "Hello, " ++ s

triple x = x * 3

half = (/2)

square x = x * x

square' = (^2)

multPi = (pi *)

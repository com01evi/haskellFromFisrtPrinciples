module Chapter2
    ( sayHello
    , triple
    , square
    , square'
    , multPi
    , printInc
    , waxOn
    , waxOn'
    ) where

sayHello :: String -> IO ()
sayHello s = print $ "Hello, " ++ s

triple x = x * 3

half = (/2)

square x = x * x

square' = (^2)

multPi = (pi *)

printInc n = print $ plusTwo n
  where plusTwo =  (+ 2)

z = 7
x= y ^ 2
waxOn = x * 5
y = z * 8

waxOn' :: Int
waxOn' = x * 5
  where
    x = y ^ 2
    y = z * 8
    z = 7

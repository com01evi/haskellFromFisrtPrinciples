module Chapter28.BasicLib(
main,
(!?)
)where

import Criterion.Main
import Data.Char
import Debug.Trace

infixl 9 !?

(!?) :: [a] -> Int -> Maybe a
xs !? n 
  | n < 0 = Nothing
  | otherwise = foldr (\x r k ->
                      case k of
                        0 -> Just x
                        _ -> r (k-1))
                (const Nothing) xs n


charToInt :: Char -> Int
charToInt = read . show

myList :: [Int]
myList = trace "myList was evaluated" ([1..9999] ++ [undefined])

main :: IO ()
main = defaultMain [ bench "index list 9999"
                     $ whnf (myList !!) 9998
                   , bench "index list maybe index 9999"  
                     $ nf (myList !?) 9999]




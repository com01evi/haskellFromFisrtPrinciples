module Chapter13.Hello
    (
      sayHello,
      concatUserInput
    ) where

sayHello :: String -> IO ()
sayHello name = putStrLn $ "Hi "++ name ++ "!"

concatUserInput :: IO String
concatUserInput = putStr "type the first name: " >> getLine >>= (\s1 -> (putStr "type second name: " >> getLine >>= (\s2 -> return (s1 ++ s2))))

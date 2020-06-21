{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Chapter30.Exception(
  runDisc,
  writeMain,
  writeMain2,
  willIFail,
  willFail,
  willIFail',
  getArgsMain
)where

import Control.Exception
import Data.Typeable
import System.Environment (getArgs)

data MyException = forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n = case n of
  0 -> Left (MyException DivideByZero)
  1 -> Left (MyException StackOverflow)
  _ -> Right n

data SomeError = Arith ArithException | Async AsyncException | SomethingElse deriving (Show)

discriminateError :: MyException -> SomeError 
discriminateError (MyException e) = 
  case cast e of
    (Just DivideByZero) -> Arith DivideByZero
    Nothing -> case cast e of
                 (Just StackOverflow) -> Async StackOverflow
                 Nothing -> SomethingElse
  
runDisc n = either discriminateError (const SomethingElse) (multiError n)

writeMain :: IO ()
writeMain = do
  writeFile "zzz" "hi"
  putStrLn "wrote to file"

handler :: SomeException -> IO ()
handler (SomeException e) = do
  putStrLn $ "we errored! It was: " ++ show e
  writeFile "bbb" "hi"

writeMain2 :: IO ()
writeMain2 = writeFile "zzz" "hi" `catch` handler

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()
    
willFail :: Integer -> IO ()
willFail denom = onlyReportError $ willIFail denom

willIFail' :: Integer -> IO ()
willIFail' denom = print (div 5 denom) `catch` handler
  where handler :: ArithException -> IO ()
        handler e = print e

testDiv :: String -> IO ()
testDiv d = onlyReportError $ willIFail $ read d

getArgsMain :: IO ()
getArgsMain = do
  args <- getArgs
  mapM_ testDiv args

myMapM :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
myMapM f t = do
  foldMap f t
  return ()

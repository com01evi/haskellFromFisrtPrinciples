{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Chapter30.Exception(
  runDisc,
  writeMain,
  writeMain2,
  willIFail,
  willFail,
  willIFail',
  getArgsMain,
  canICatch,
  SomeError(..),
  onlyReportError,
  arithMain
)where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import Data.Typeable
import System.Environment (getArgs)
import System.Random (randomRIO)

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

canICatch :: (Exception e) => e -> IO (Either SomeException ())
canICatch e = try $ throwIO e

randomException :: IO ()
randomException = do
  i <- randomRIO (1,10 :: Int)
  if i `elem` [1..9]
    then throwIO DivideByZero
    else throwIO StackOverflow

arithMain :: IO ()
arithMain = forever $ do
  let tryS :: IO () -> IO (Either SomeException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "Live to loop another day!!!!"
  threadDelay (1 * 1000000)

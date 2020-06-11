{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Chapter30.Exception(
  runDisc
)where

import Control.Exception(ArithException(..), AsyncException(..))
import Data.Typeable

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

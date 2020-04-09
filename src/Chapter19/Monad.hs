{-# LANGUAGE OverloadedStrings #-}

module Chapter19.Monad(
  shortyGen
 ,x
)where

import Control.Monad(replicateM)
import Control.Monad.IO.Class(liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = (length xs) - 1
  index <- SR.randomRIO (0, maxIndex)
  return (xs !! index)

myreplicateM :: Monad m => Int -> m a -> m [a]
myreplicateM 0 _ = return []
myreplicateM n m = do
  (:) <$> m <*> myreplicateM (n-1) m

shortyGen :: IO String
shortyGen = myreplicateM 7 $ randomElement alphaNum

x = myreplicateM 4 [1,2,3]

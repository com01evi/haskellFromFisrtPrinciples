{-# LANGUAGE OverloadedStrings #-}
module Chapter26.Scotty(
liftmain
)where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class

liftmain = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]



{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Chapter31.Debug(
  sockMain
)where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString ( recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (soc, _) <- accept sock
  printAndKickback soc
  close soc
  where printAndKickback conn = do
          msg <- recv conn 1024
          print msg 
          sendAll conn msg

sockMain :: IO ()
sockMain = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                           Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  logAndEcho sock
  close sock
          

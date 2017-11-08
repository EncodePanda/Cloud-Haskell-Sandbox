{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module PingPong
  (      pingPongMain
  ) where

import Logger (logInfo)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)
import Data.Binary
import Data.Typeable
import GHC.Generics

{-|
ping pong implemented with dedicated Messages
-}

data Protocol = Ping ProcessId
             | Pong
  deriving (Typeable, Generic, Show)

instance Binary Protocol

ping :: Process ()
ping = do
  pingPid <- getSelfPid
  pongPid <- spawnLocal pong
  forever $ do
      _ <- send pongPid (Ping pingPid)
      logInfo "ping sent"
      Pong <- expect
      logInfo "pong received"
      liftIO $ threadDelay 1000000
  
pong :: Process ()
pong = forever $ do
  myPid <- getSelfPid
  Ping pid <- expect
  logInfo "ping received"
  send pid Pong
  logInfo "pong sent"
  liftIO $ threadDelay 1000000


pingPongMain :: IO ()
pingPongMain = do
  Right t <- createTransport "127.0.0.1" "10501" ((,) "127.0.0.1") defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node ping

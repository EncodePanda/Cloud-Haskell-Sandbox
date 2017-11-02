{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module PingPong
  (      pingPongMain
  ) where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary
import Data.Typeable
import GHC.Generics

{-|
ping pong implemented with dedicated Messages
-}

data Protocol = Ping ProcessId
             | Pong ProcessId
             | Done
  deriving (Typeable, Generic, Show)

instance Binary Protocol

forever :: ProcessId -> ProcessId -> Process ()
forever pingPid pongPid = do
  _ <- liftIO $ putStrLn "ping"
  _ <- send pongPid (Ping pingPid)
  Pong pid <- expect
  forever pingPid pongPid

ping :: Process ()
ping = do
  pingPid <- getSelfPid
  pongPid <- spawnLocal pong
  forever pingPid pongPid
  
pong :: Process ()
pong = do
  myPid <- getSelfPid
  Ping pid <- expect
  send pid (Pong myPid)
  _ <- liftIO $ putStrLn "pong"
  pong

pingPongMain :: IO ()
pingPongMain = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node ping

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

ping :: ProcessId -> Process ()
ping ppPid = do
  selfPid <- getSelfPid
  pongPid <- expect :: Process ProcessId
  _ <- send pongPid (Ping selfPid)
  Pong pongPid <- expect
  _ <- liftIO $ putStrLn $ "pong received from " ++ (show pongPid)
  send ppPid "done"
  
pong :: ProcessId -> Process ()
pong pingPid = do
  selfPid <- getSelfPid
  Ping pid <- expect
  _ <- liftIO $ putStrLn $ "ping received from " ++ (show pid)
  send pid (Pong selfPid)

pingpong :: Process ()
pingpong = do
  ppPid <- getSelfPid
  pingPid <- spawnLocal $ ping ppPid
  pongPid <- spawnLocal $ pong pingPid
  _ <- send pingPid pongPid
  _ <- expect :: Process String
  return ()

pingPongMain :: IO ()
pingPongMain = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node pingpong

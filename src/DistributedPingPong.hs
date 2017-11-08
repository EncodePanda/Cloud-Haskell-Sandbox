{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}

module DistributedPingPong
  ( pingPongMain
  ) where

import System.Environment (getArgs)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Monad (forever, forM)
import Data.Binary
import Data.Typeable
import GHC.Generics

data Protocol = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic, Show)

instance Binary Protocol

logInfo :: [Char] -> Process ()
logInfo msg = liftIO . putStrLn $ msg

pong :: Process ()
pong = forever $ do
  myPid <- getSelfPid
  Ping pid <- expect
  logInfo  "ping received"
  send pid (Pong myPid)
  logInfo "pong sent"

remotable ['pong]

spawnFor :: [NodeId] -> Closure (Process ()) -> Process [ProcessId]
spawnFor nodes c = forM nodes $ \nid -> spawn nid c

expectPongs :: [ProcessId] -> Process ()
expectPongs [] = return ()
expectPongs ps = do
  Pong pid <- expect
  expectPongs $ filter (/= pid) ps

pingPong :: Backend -> [NodeId] -> Process ()
pingPong backend peers = do
  myPid <- getSelfPid
  logInfo $ "Peers: " ++ show peers
  ps <- spawnFor peers $(mkStaticClosure 'pong)
  forever $ do
    _ <- forM ps $ \pid -> do
            send pid (Ping myPid)
            logInfo "ping sent" 
    expectPongs ps
    logInfo "all pongs received"

pingPongMain :: IO ()
pingPongMain = do
  args <- getArgs

  let rtable = __remoteTable initRemoteTable

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend (pingPong backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> putStrLn "exit"

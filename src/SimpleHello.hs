module SimpleHello
    ( simpleHelloWorld
    ) where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

-- simple hello send to self, received and printed
hello :: Process ()
hello = do
  self <- getSelfPid
  send self "hello"
  hello <- expect :: Process String
  liftIO $ putStrLn hello

simpleHelloWorld :: IO ()
simpleHelloWorld = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node hello

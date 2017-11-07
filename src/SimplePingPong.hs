module SimplePingPong
  (      pingPongMain
  ) where


import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

{-|
   ping pong implemented with String messages
-}
ping :: ProcessId -> Process ()
ping ppPid = do
  pongPid <- expect :: Process ProcessId
  _ <- send pongPid "ping"
  msg <- expect :: Process String
  _ <- liftIO $ putStrLn msg
  send ppPid "done"
  
pong :: ProcessId -> Process ()
pong pingPid = do
  msg <- expect :: Process String
  _ <- liftIO $ putStrLn msg
  send pingPid "pong"

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
  Right t <- createTransport "127.0.0.1" "10501" ((,) "127.0.0.1") defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node pingpong

module Logger
  (
    logInfo
  ) where

import Control.Distributed.Process

logInfo :: [Char] -> Process ()
logInfo msg = liftIO . putStrLn $ msg

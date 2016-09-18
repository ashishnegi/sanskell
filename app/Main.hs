module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Concurrent as Con
import qualified Data.Map as M

import qualified Sanskell.Api as A
import qualified Sanskell.Types as ST
import qualified Sanskell.Server as SS

start :: Warp.Port -> IO ()
start port = do
  nextJobId   <- Con.newMVar (ST.JobId 1)
  jobResult   <- Con.newMVar M.empty
  pendingJobs <- Con.newMVar []
  jobChan     <- Con.newChan

  let server = ST.Server nextJobId jobResult pendingJobs jobChan
  SS.startServer server

  Warp.run port (A.app server)

main :: IO ()
main = start 8034

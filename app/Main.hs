module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Sanskell.Api as A
import qualified Sanskell.Types as ST
import qualified Control.Concurrent as Con
import qualified Data.Map as M

start :: Warp.Port -> IO ()
start port = do
  nextJobId   <- Con.newMVar (ST.JobId 1)
  jobResult   <- Con.newMVar M.empty
  jobChan     <- Con.newChan
  
  let server = ST.Server nextJobId jobResult jobChan
  Warp.run port (A.app server)

main :: IO ()
main = start 8034

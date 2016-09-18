module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Sanskell.Api as A
import qualified Sanskell.Types as ST
import qualified Control.Concurrent as Con
import qualified Data.Map as M

start :: Warp.Port -> IO ()
start port = do
  pendingJobs <- Con.newMVar []
  jobResult   <- Con.newMVar M.empty
  let server = ST.Server pendingJobs jobResult
  Warp.run port (A.app server)

main :: IO ()
main = start 8034

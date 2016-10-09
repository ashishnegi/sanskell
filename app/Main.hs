{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Concurrent as Con
import qualified Data.Map as M
import qualified Data.Configurator as DC
import qualified Data.Maybe as DMay

import qualified Sanskell.Api as A
import qualified Sanskell.Types as ST
import qualified Sanskell.Server as SS

start :: ST.Config -> IO ()
start serverConfig = do
  nextJobId   <- Con.newMVar (ST.JobId 1)
  jobResult   <- Con.newMVar M.empty
  pendingJobs <- Con.newMVar []
  jobChan     <- Con.newChan

  let server = ST.Server nextJobId jobResult pendingJobs jobChan serverConfig
  SS.startServer server

  Warp.run (ST.port serverConfig) $ A.app server

main :: IO ()
main = do
  config      <- DC.load [ DC.Required "config/dev/config.cfg" ]
  appPort     <- DC.lookup config "app.port"

  let serverConfig = ST.Config (DMay.fromMaybe 8034 appPort)
  start serverConfig

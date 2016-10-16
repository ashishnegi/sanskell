{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Concurrent as Con
import qualified Data.Map as M
import qualified Data.Configurator as DC
import qualified Data.Maybe as DMay
import qualified System.Environment as Env
import qualified Data.Char as C
import qualified Data.UUID.V4 as DU
import qualified Network.Wai.Middleware.Gzip as Gzip

import qualified Sanskell.Api as A
import qualified Sanskell.Types as ST
import qualified Sanskell.Server as SS

start :: ST.Config -> IO ()
start serverConfig = do
  uuid        <- DU.nextRandom
  nextJobId   <- Con.newMVar $ ST.JobId uuid
  jobResult   <- Con.newMVar M.empty
  pendingJobs <- Con.newMVar []
  jobChan     <- Con.newChan

  let server = ST.Server nextJobId jobResult pendingJobs jobChan serverConfig
  SS.startServer server

  let application = Gzip.gzip (Gzip.def { Gzip.gzipFiles = Gzip.GzipCompress}) $ A.app server
  Warp.run (ST.port serverConfig) application

main :: IO ()
main = do
  envFromEnv  <- Env.lookupEnv "AppEnv"

  let env = DMay.fromMaybe "dev" envFromEnv
      configFileName = "config/" ++ (map C.toLower env) ++ "/app.cfg"

  config     <- DC.load [ DC.Required configFileName ]
  appPortStr <- DC.lookup config "app.port"
  appRootUrl <- DC.lookup config "app.root-url"

  let appPort = DMay.fromMaybe 8034 (read <$> appPortStr)
      rootUrl = DMay.fromMaybe ("http://localhost:" ++ show appPort) appRootUrl
      serverConfig = ST.Config appPort (ST.URL rootUrl)

  putStrLn $ "Running server on " ++ (show rootUrl)

  start serverConfig

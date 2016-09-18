{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Sanskell.Api (app) where

import qualified Network.Wai as NW
import qualified Servant as S
import Servant ((:>), (:<|>))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Sanskell.Types as ST
import qualified Sanskell.Server as SS

import Debug.Trace

type JobApi = "job" :> S.Capture "id" ST.JobId :> S.Get '[S.JSON] (Maybe ST.JobResult )
         :<|> "job" :> S.ReqBody '[S.JSON] ST.JobPostBody :> S.Post '[S.JSON] (Either T.Text ST.JobId)

jobApi :: S.Proxy JobApi
jobApi = S.Proxy

serverRouter :: ST.Server -> S.Server JobApi
serverRouter server = jobGet
    S.:<|> jobPost
  where
    jobGet :: ST.JobId -> S.Handler (Maybe ST.JobResult)
    jobGet id = return $ SS.jobResult server id

    jobPost :: ST.JobPostBody -> S.Handler (Either T.Text ST.JobId)
    jobPost ST.JobPostBody{..} = traceShow jobUrl $ return $ SS.addJob server jobUrl

app :: ST.Server -> NW.Application
app server = S.serve jobApi (serverRouter server)

instance S.FromHttpApiData ST.JobId where
  parseUrlPiece t = ST.JobId <$> S.parseUrlPiece t

instance A.ToJSON ST.JobId
instance A.ToJSON ST.JobResult
instance A.FromJSON ST.JobPostBody

-- $ curl -X POST -d '{"jobUrl":"http://www.google.com"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8034/job

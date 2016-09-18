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

import Debug.Trace

type JobApi = "job" :> S.Capture "id" ST.JobId :> S.Get '[S.JSON] [ ST.JobResult ]
         :<|> "job" :> S.ReqBody '[S.JSON] ST.JobPostBody :> S.Post '[S.JSON] (Either T.Text ST.JobId)

jobApi :: S.Proxy JobApi
jobApi = S.Proxy

server :: S.Server JobApi
server = jobGet
    S.:<|> jobPost
  where
    jobGet :: ST.JobId -> S.Handler [ST.JobResult]
    jobGet id = return [ ST.JobResult id M.empty ]

    jobPost :: ST.JobPostBody -> S.Handler (Either T.Text ST.JobId)
    jobPost ST.JobPostBody{..} = traceShow jobUrl $ return . Right . ST.JobId $ 1

app :: NW.Application
app = S.serve jobApi server

instance S.FromHttpApiData ST.JobId where
  parseUrlPiece t = ST.JobId <$> S.parseUrlPiece t

instance A.ToJSON ST.JobId
instance A.ToJSON ST.JobResult
instance A.FromJSON ST.JobPostBody

-- $ curl -X POST -d '{"jobUrl":"http://www.google.com"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8034/job

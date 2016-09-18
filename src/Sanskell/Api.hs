{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sanskell.Api (app) where

import qualified Network.Wai as NW
import qualified Servant as S
import Servant ((:>), (:<|>))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Sanskell.Types as ST

type JobApi = "job" :> S.Capture "id" ST.JobId :> S.GetNoContent '[S.JSON] [ST.JobResult]
        --  :<|> "job" :> S.QueryParam "url" String :> S.PostNoContent '[S.JSON] []


jobApi :: S.Proxy JobApi
jobApi = S.Proxy

server :: S.Server JobApi
server = jobGet
  where
    jobGet :: ST.JobId -> S.Handler [ST.JobResult]
    jobGet id = return [ ST.JobResult id M.empty ]

app :: NW.Application
app = S.serve jobApi server

instance S.FromHttpApiData ST.JobId where
  parseUrlPiece t = ST.JobId <$> S.parseUrlPiece t

instance A.ToJSON ST.JobId
instance A.ToJSON ST.JobResult

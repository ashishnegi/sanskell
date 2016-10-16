{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Sanskell.Api (app, jobApi) where

import qualified Network.Wai as NW
import qualified Servant as S
import Servant ((:>), (:<|>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.UUID as DU
import qualified Sanskell.Types as ST
import qualified Sanskell.Server as SS
import qualified Control.Monad.IO.Class as CM
import qualified Servant.Elm as SElm
import qualified Elm as Elm

import Debug.Trace

type JobApi = "job" :> "status" :> S.Capture "id" ST.JobId :> S.Get '[S.JSON] ST.JobStatus
         :<|> "job" :> S.ReqBody '[S.JSON] ST.JobPostBody :> S.Post '[S.JSON] ST.JobId
         :<|> "job" :> S.Capture "id" ST.JobId :> S.Get '[S.JSON] ST.JobResult
         :<|> "jobs" :> S.Get '[ S.JSON ] [ ST.JobId ]
         :<|> S.Raw

jobApi :: S.Proxy JobApi
jobApi = S.Proxy

serverRouter :: ST.Server -> S.Server JobApi
serverRouter server = statusGet
    S.:<|> jobPost
    S.:<|> jobGet
    S.:<|> jobsList
    S.:<|> S.serveDirectory "assets"
  where
    jobGet :: ST.JobId -> S.Handler ST.JobResult
    jobGet jid = do
      res <- CM.liftIO $ SS.jobResult server jid
      case res of
        Left e -> S.throwError (S.err404 { S.errBody = TLE.encodeUtf8 . TL.fromStrict $ e })
        Right v -> return v

    jobPost :: ST.JobPostBody -> S.Handler ST.JobId
    jobPost ST.JobPostBody{..} = do
      CM.liftIO $ putStrLn . show $ jobUrl
      res <- CM.liftIO $ SS.addJob server jobUrl
      case res of
        Left e -> S.throwError e
        Right v -> return v

    statusGet :: ST.JobId -> S.Handler ST.JobStatus
    statusGet jid = do
      CM.liftIO $ putStrLn . show $ jid
      res <- CM.liftIO $ SS.jobStatus server jid
      case res of
        Left e -> S.throwError e
        Right v -> return v

    jobsList :: S.Handler [ ST.JobId ]
    jobsList = do
      CM.liftIO $ SS.jobsList server

app :: ST.Server -> NW.Application
app server = S.serve jobApi (serverRouter server)

-- app :: NW.Application -> NW.Application -> NW.Application
-- app staticApp apiApp req respond =
--   staticApp req (\ response ->
--                   do
--                     let status = Status.statusCode . NW.responseStatus $ response
--                     if (traceShow status status) < 400
--                     then respond response
--                     else apiApp req respond)

instance S.FromHttpApiData ST.JobId where
  parseUrlPiece t =
    case DU.fromText t of
      Nothing -> Left "Could not parse uuid"
      Just t' -> Right $ ST.JobId t'

instance Elm.ElmType ST.JobId where
  toElmType (ST.JobId jId) = Elm.Primitive "String"

instance SElm.ElmType ST.JobStatus
instance SElm.ElmType ST.Message
instance SElm.ElmType ST.Error
instance SElm.ElmType ST.URL
instance SElm.ElmType ST.JobState
instance SElm.ElmType ST.JobPostBody
instance SElm.ElmType ST.JobResult
-- instance (SElm.ElmType a, SElm.ElmType b) => SElm.ElmType (Either a b) where
--   toElmType eab = case eab of
--     Left a -> Elm.Product (Elm.Primitive "Left") (Elm.toElmType a)
--     Right b -> Elm.Product (Elm.Primitive "Right") (Elm.toElmType b)

-- $ curl -X POST -d '{"jobUrl":"http://www.google.com"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8034/job

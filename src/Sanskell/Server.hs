{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Sanskell.Server where

import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai

import qualified Network.URI as NU
import qualified Data.Text as T
import qualified Control.Concurrent as Con
import qualified Data.Map as M
import qualified Control.Monad as CM (forever, void)
import qualified Servant as S
import qualified Data.List as DL
import qualified Sanskell.Types as ST
import qualified Sanskell.Words as SW

startServer :: ST.Server -> IO ()
startServer ST.Server{..} = do
  CM.void $ Con.forkIO $ CM.forever $ do
    (jobId, url) <- Con.readChan jobChan
    -- block to serve one crawl request at a time..
    wordMap <- SW.wordCloudOfWebsite url jobId
    let jobRes = ST.JobResult jobId <$> wordMap
    Con.modifyMVar_ jobResults (\m -> return . M.insertWith changeResult jobId jobRes $ m)
    -- remove from pending request
    Con.modifyMVar_ pendingJobs (return . DL.delete jobId)
  where
    changeResult v1 v2 = case (v1, v2) of
       -- do not update if v2 failed.
      (Right _, Left _) -> v1
      _                 -> v2

staticApp :: Network.Wai.Application
staticApp = Static.staticApp . Static.defaultWebAppSettings $ "assets"

addJob :: ST.Server -> String -> IO (Either S.ServantErr ST.JobId)
addJob ST.Server{..} url = do
  let parsedUri = NU.parseURI url
  maybe (return . Left $ (S.err400 { S.errBody = "Bad Url" }))
    (\ _ -> do
        jid <- Con.modifyMVar nextJobId $ \ (ST.JobId jid) -> return . (\a -> (a,a)) $ ST.JobId (jid + 1)
        -- add to pending list
        Con.modifyMVar_ pendingJobs (return . (:) jid)
        -- put on a channel that would be read sequentially..
        -- should be passing parsedUri => Just uri
        Con.writeChan jobChan ( jid, url )
        return $ Right jid)
    parsedUri

jobResult :: ST.Server -> ST.JobId -> IO (Either T.Text ST.JobResult)
jobResult ST.Server{..} jobId = do
  results <- Con.readMVar jobResults
  let v = M.lookup jobId results
  case v of
    Nothing -> return . Left . T.pack $ "No such url exists."
    Just v' -> return v'

jobStatus :: ST.Server -> ST.JobId -> IO (Either S.ServantErr ST.JobStatus)
jobStatus ST.Server{..} jobId = do
  pJobs <- Con.readMVar pendingJobs
  let pJob = DL.find (== jobId) pJobs
  case pJob of
    -- in pending list..
    Just _ -> return . Right $ ST.JobStatus jobId (ST.Message (mkJobStatusUrl config jobId)) ST.Pending

    -- not in pending list
    Nothing -> do
      completedJobs <- Con.readMVar jobResults
      let job = M.lookup jobId completedJobs
      case job of
        -- not in completed list either
        Nothing -> return . Left $ S.err404
        -- in completed list.
        Just j' ->
          case j' of
            -- failed to complete..
            Left err -> return . Right $ ST.JobStatus jobId (ST.Message . T.unpack $ err) ST.Failed
            -- job finished successfully..
            Right _   -> return . Right $ ST.JobStatus jobId (ST.Message (mkJobUrl config jobId)) ST.Finished


mkJobUrl :: ST.Config -> ST.JobId -> String
mkJobUrl (ST.Config { rootUrl = (ST.URL url)}) (ST.JobId jobId) =
  url ++ "/?job-id=" ++ (show jobId)

mkJobStatusUrl :: ST.Config -> ST.JobId -> String
mkJobStatusUrl (ST.Config { rootUrl = (ST.URL url)}) (ST.JobId jobId) =
  url ++ "/job/status/" ++ (show jobId)

{-# LANGUAGE RecordWildCards #-}
module Sanskell.Server where

import qualified Network.URI as NU
import qualified Data.Text as T
import qualified Control.Concurrent as Con
import qualified Data.Map as M
import qualified Control.Monad as CM (forever, void)

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
  where
    changeResult v1 v2 = case (v1, v2) of
       -- do not update if v2 failed.
      (Right _, Left _) -> v1
      _                 -> v2

addJob :: ST.Server -> String -> IO (Either T.Text ST.JobId)
addJob ST.Server{..} url = do
  let parsedUri = NU.parseURI url
  maybe (return . Left . T.pack $ "Bad url")
    (\ _ -> do
        -- put on a channel that would be read sequentially..
        id <- Con.modifyMVar nextJobId $ \ (ST.JobId jid) -> return . (\a -> (a,a)) $ ST.JobId (jid + 1)
        -- should be passing parsedUri => Just uri
        Con.writeChan jobChan ( id, url )
        return $ Right id)
    parsedUri

jobResult :: ST.Server -> ST.JobId -> IO (Either T.Text ST.JobResult)
jobResult ST.Server{..} jobId = do
  results <- Con.readMVar jobResults
  let v = M.lookup jobId results
  case v of
    Nothing -> return . Left . T.pack $ "Wrong Id"
    Just v' -> return v'

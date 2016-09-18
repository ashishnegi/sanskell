{-# LANGUAGE RecordWildCards #-}
module Sanskell.Server where

import qualified Network.URI as NU
import qualified Sanskell.Types as ST
import qualified Data.Text as T
import qualified Control.Concurrent as Con
import qualified Data.Map as M

addJob :: ST.Server -> String -> IO (Either T.Text ST.JobId)
addJob ST.Server{..} url = do
  let parsedUri = NU.parseURI url
  maybe (return . Left . T.pack $ "Bad url")
    (\ _ -> do
        -- put on a channel that would be read sequentially..
        id <- Con.modifyMVar nextJobId $ \ (ST.JobId id) -> return . (\a -> (a,a)) $ ST.JobId (id + 1)
        -- should be passing parsedUri => Just uri
        Con.writeChan jobChan ( id, url )
        return $ Right id)
    parsedUri

jobResult :: ST.Server -> ST.JobId -> IO (Maybe ST.JobResult)
jobResult ST.Server{..} jobId = do
  results <- Con.readMVar jobResults
  return . M.lookup jobId $ results

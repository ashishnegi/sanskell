{-# LANGUAGE RecordWildCards #-}

module Sanskell.Crawler where

import qualified Network.URI as NU
import qualified Control.Concurrent as Con
import qualified Control.Concurrent.STM as CStm
import qualified Control.Concurrent.Async as A
import qualified Data.Text as T
import qualified Text.HTML.Scalpel as SP

import Data.Maybe (catMaybes)
import Control.Monad (replicateM_, forM_, when, replicateM)
import qualified Sanskell.Types as ST

data Job = Job ST.Link | JobEnded
type LinkQueue = CStm.TQueue Job

-- one crawling thread in app :
--  should fork as many scrappingThreads as in config.
--  Load distribution among scrappingThread(s) : each thread eagerly takes work.
crawlingThread :: ST.CrawlConfig -> ST.Link -> ST.CrawlResultChan -> IO ()
crawlingThread ST.CrawlConfig{..} baseLink outChan = do
  inQueue <- CStm.atomically $ CStm.newTQueue
  CStm.atomically $ CStm.writeTQueue inQueue (Job baseLink)
  -- fork numThreds threds.
  asyncWorks <- replicateM numThreads $ A.async $ scrappingThread (inQueue, outChan)
  -- as consumers / scrappingThread are slow, queue emptiness means that we are done..
  -- if even one of the thread returns this means work is finished.
  replicateM_ numThreads $ CStm.atomically $ CStm.writeTQueue inQueue JobEnded
  -- wait for all threads to finish
  mapM_ A.wait asyncWorks
  -- tell caller that work has finished.
  Con.writeChan outChan ST.CrawlFinished

--  puts Result on CrawlResult Channel
scrappingThread :: (LinkQueue, ST.CrawlResultChan) -> IO ()
scrappingThread chans@(inQueue, outChan) = do
  job <- CStm.atomically $ CStm.readTQueue inQueue
  case job of
    Job (ST.Link jobId depthRemaining linkUri) -> do
      let link = NU.uriToString id linkUri $ ""
      texts <- SP.scrapeURL link $ SP.texts SP.Any
      links <- SP.scrapeURL link $ SP.attrs "href" SP.Any

      let normalizedLinks = catMaybes $ maybe [] (fmap $ normalizeLink linkUri) links
      let crawlResult = ST.CrawlResult jobId linkUri <$> texts
      maybe (return ()) (Con.writeChan outChan) crawlResult

      when (depthRemaining > 0) $
        CStm.atomically $ forM_ normalizedLinks $ CStm.writeTQueue inQueue . Job . ST.Link jobId (depthRemaining - 1)
      -- as consumers / scrappingThread are slow, queue emptiness means that we are done..
      emptyQueue <- CStm.atomically $ CStm.isEmptyTQueue inQueue
      when (not emptyQueue) $ scrappingThread chans
    JobEnded -> return () -- do not recurse
  where
    normalizeLink :: NU.URI -> T.Text -> Maybe NU.URI
    normalizeLink base newUrl = if T.length newUrl > 0
                                then flip NU.relativeTo base <$> NU.parseURIReference (T.unpack newUrl)
                                else Nothing

-- test :: IO ()
-- test = do
--   let uri = Data.Maybe.fromJust $ NU.parseURI "https://www.facebook.com"
--   chan <- Con.newChan
--   (crawlingThread (ST.CrawlConfig 2) (ST.Link (ST.JobId 1) 1 uri) chan) >> return ()

{-# LANGUAGE RecordWildCards #-}

module Sanskell.Crawler where

import qualified Network.URI as NU
import qualified Control.Concurrent as Con
import qualified Control.Concurrent.STM as CStm
import qualified Control.Concurrent.Async as A
import qualified Data.Text as T
import qualified Text.HTML.Scalpel as SP

import Data.Maybe (catMaybes, fromJust)
import Control.Monad (forM_, when, replicateM)
import Control.Monad.Fix (fix)
import qualified Sanskell.Types as ST

import Debug.Trace

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
  -- wait for all threads to finish
  mapM_ A.wait asyncWorks

  putStrLn "Waiting done!!!"
  remainingWork <- flushQueue inQueue
  -- One JobEnded Msg would be there. :P
  when (length remainingWork > 1) $ putStrLn "Bad.. Could not finish the work"

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

      let validLinks = catMaybes $ maybe [] (fmap $ validizeLink linkUri) links
      let crawlResult = ST.CrawlResult jobId linkUri <$> texts
      maybe (return ()) (Con.writeChan outChan) crawlResult

      when (depthRemaining > 0) $
        CStm.atomically $ forM_ validLinks $ CStm.writeTQueue inQueue . Job . ST.Link jobId (depthRemaining - 1)
      -- as consumers / scrappingThread are slow, queue emptiness means that we are done..
      emptyQueue <- CStm.atomically $ CStm.isEmptyTQueue inQueue

      threadId <- Con.myThreadId
      putStrLn . show $ ("ThreadId: ", threadId, "Done: ", link, " Empty Queue: ", emptyQueue)
      -- as consumers / scrappingThread are slow, queue emptiness means that we are done..
      -- if even one of the thread returns this means work is finished.
      -- tell other threads to stop as well
      if emptyQueue
      then CStm.atomically $ CStm.writeTQueue inQueue JobEnded
      else scrappingThread chans

    -- do not recurse
    JobEnded -> do
      threadId <- Con.myThreadId
      putStrLn . show $ ("ThreadId: ", threadId, " received JobEnded")
      CStm.atomically $ CStm.writeTQueue inQueue JobEnded
  where
    validizeLink :: NU.URI -> T.Text -> Maybe NU.URI
    validizeLink base newUrl =
      if T.length newUrl > 0
      then let v = flip NU.relativeTo base <$> NU.parseURIReference (T.unpack newUrl)
               v2 = sameDomainURI (NU.uriAuthority base) v
           in traceShow (v,v2) v2
      else Nothing

    sameDomainURI :: Maybe NU.URIAuth -> Maybe NU.URI -> Maybe NU.URI
    sameDomainURI baseUriAuth maybeUri = do
      uri <- maybeUri
      auth <- NU.uriAuthority uri
      baseUriRegName <- baseUriAuth
      if NU.uriRegName baseUriRegName == NU.uriRegName auth
      then return uri
      else Nothing

flushQueue :: CStm.TQueue a -> IO [a]
flushQueue channel = CStm.atomically $ readAll channel
  where readAll c = do
          emptyChan <- CStm.isEmptyTQueue c
          if emptyChan
          then return []
          else (:) <$> CStm.readTQueue c <*> readAll c

test :: IO ()
test = do
  let uri = fromJust $ NU.parseURI "https://jaspervdj.be/hakyll/"
  chan <- Con.newChan
  threadId <- Con.forkIO $ crawlingThread (ST.CrawlConfig 2) (ST.Link (ST.JobId 1) 1 uri) chan

  fix $ \loop -> do
    m <- Con.readChan chan
    case m of
      ST.CrawlFinished -> return ()
      ST.CrawlResult _ url _ -> (putStrLn $ NU.uriToString id url $ "") >> loop

  Con.killThread threadId

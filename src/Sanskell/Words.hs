module Sanskell.Words (wordCloudOfWebsite) where

import qualified Network.URI as NU
import qualified Control.Concurrent as Con
import qualified Sanskell.Crawler as SC
import qualified Sanskell.Types as ST
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Foldable as DF
import qualified Data.Char as DC

import Debug.Trace

wordCloudOfWebsite :: String -> ST.JobId -> IO (Either T.Text (M.Map T.Text Integer))
wordCloudOfWebsite url jobId = do
  texts <- textsOnWebsite url jobId
  return $ wordCount <$> texts

textsOnWebsite :: String -> ST.JobId -> IO (Either T.Text [ T.Text ])
textsOnWebsite url jobId = do
  let parsedUri = NU.parseURI url

  maybe (return . Left . T.pack $ "Parsing of url failed") (\ uri -> do
      let levelsDeep = 2
          maxPagesToCrawl = 10
          config = ST.CrawlConfig levelsDeep maxPagesToCrawl
          link = ST.Link jobId 3 uri

      crawlResultChan <- Con.newChan
      crawlingThread <- Con.forkIO $ SC.crawlingThread config link crawlResultChan

      traceShow ("processing ", uri, jobId) $ return ()
      allTexts <- joinAllTexts crawlResultChan []
      Con.killThread crawlingThread
      return $ Right allTexts)
    parsedUri

  where
    joinAllTexts crawlResultChan t = do
      m <- Con.readChan crawlResultChan
      traceShow "getting msgs " $ return ()
      case m of
        ST.CrawlFinished -> return t
        ST.CrawlResult _ _ text -> joinAllTexts crawlResultChan $ (T.concat text) : t

wordCount :: [ T.Text ] -> M.Map T.Text Integer
wordCount texts = DF.foldl' countWords M.empty texts
  where
    countWords m text =
      let words = T.split DC.isSeparator text
      in DF.foldl' (\m' word -> M.insertWith (+) word 1 m') m words

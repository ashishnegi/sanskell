{-# LANGUAGE DeriveGeneric #-}

module Sanskell.Types where

import qualified Network.URI as NU (URI)
import qualified Control.Concurrent as Con (Chan)
import qualified Data.Text as T (Text)
import qualified Data.Map as M
import GHC.Generics

newtype JobId = JobId Int deriving (Eq, Show, Generic)

type DepthRemaining = Int

data Link = Link JobId DepthRemaining NU.URI

data CrawlResult = CrawlResult { jobId    :: JobId
                               , url      :: NU.URI
                               , bodyText :: [ T.Text ]
                               }
                 | CrawlFinished

type CrawlResultChan = Con.Chan CrawlResult

data CrawlConfig =
  CrawlConfig
  { numThreads :: Int
  }

data JobResult =
  JobResult
  { resultJobId :: JobId
  , wordsCount  :: M.Map T.Text Integer
  }
  deriving (Eq, Show, Generic)

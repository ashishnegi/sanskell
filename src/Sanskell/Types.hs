{-# LANGUAGE DeriveGeneric #-}

module Sanskell.Types where

import qualified Network.URI as NU (URI)
import qualified Control.Concurrent as Con (Chan, MVar)
import qualified Data.Text as T (Text)
import qualified Data.Map as M
import qualified Data.Aeson as A

import GHC.Generics

newtype JobId = JobId Int deriving (Eq, Ord, Show, Generic)

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
  { numThreads      :: Int
  , maxPagesToCrawl :: Int
  }

-- Words.hs
data JobResult =
  JobResult
  { resultJobId :: JobId
  -- , should have job start/finish time
  , wordsCount  :: M.Map T.Text Integer
  }
  deriving (Eq, Show, Generic)

-- Api.hs
newtype JobPostBody =
  JobPostBody
  { jobUrl :: String
  }
  deriving (Eq, Show, Generic)

newtype URL = URL String deriving (Eq, Show, Generic)

-- Server.hs
data Server =
  Server
  { nextJobId   :: Con.MVar JobId
  , jobResults  :: Con.MVar ( M.Map JobId (Either T.Text JobResult))
  , pendingJobs :: Con.MVar [ JobId ]
  , jobChan     :: Con.Chan ( JobId, String )
  }

data JobState = Pending | Finished deriving (Eq, Show, Generic)
newtype Error = Error T.Text deriving (Eq, Show, Generic)

data JobStatus =
  JobStatus
  { statusJobId  :: JobId
  -- , should have job start/finish time
  , jobResultUrl :: Either Error URL
  , jobState     :: JobState
  }
  deriving (Eq, Show, Generic)

instance A.ToJSON JobId
instance A.ToJSON JobResult
instance A.ToJSON Error
instance A.ToJSON URL
instance A.ToJSON JobStatus
instance A.ToJSON JobState
instance A.FromJSON JobPostBody

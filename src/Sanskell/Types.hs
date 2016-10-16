{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sanskell.Types where

import qualified Network.URI as NU (URI)
import qualified Control.Concurrent as Con (Chan, MVar)
import qualified Data.Text as T (Text, pack)
import qualified Data.Map as M
import qualified Data.UUID as DU
import qualified Data.Aeson as A
import Data.Aeson.Types ((.=))

import GHC.Generics

newtype JobId = JobId DU.UUID deriving (Eq, Ord, Show, Generic)

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
  , jobResultUrl :: String
  -- , should have job start/finish time
  , wordsCount  :: M.Map T.Text Integer
  }
  deriving (Eq, Show, Generic)

-- Server.hs
data Config =
  Config { port :: Int
         , rootUrl :: URL
         }

data Server =
  Server
  { nextJobId   :: Con.MVar JobId
  , jobResults  :: Con.MVar ( M.Map JobId (Either T.Text JobResult))
  , pendingJobs :: Con.MVar [ (JobId, String) ]
  , jobChan     :: Con.Chan ( JobId, String )
  , config      :: Config
  }

-- Api.hs
newtype JobPostBody =
  JobPostBody
  { jobUrl :: String
  }
  deriving (Eq, Show, Generic)

newtype URL = URL String deriving (Eq, Show, Generic)

data JobState = Pending | Finished | Failed deriving (Eq, Show, Generic)
newtype Error = Error T.Text deriving (Eq, Show, Generic)

newtype Message = Message String deriving (Eq, Show, Generic)
data JobStatus =
  JobStatus
  { statusJobId  :: JobId
  -- , should have job start/finish time
  , jobResult    :: Message
  , jobState     :: JobState
  }
  deriving (Eq, Show, Generic)

instance A.ToJSON JobId where
  toJSON (JobId jId) = A.String . DU.toText $ jId

instance A.ToJSON JobResult
instance A.ToJSON Error
instance A.ToJSON URL
instance A.ToJSON Message where
  toJSON (Message msg) = A.object [ "msg" .= msg ]

instance A.ToJSON JobStatus
instance A.ToJSON JobState
instance A.FromJSON JobPostBody

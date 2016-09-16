module Sanskell.Types (Link) where

import qualified Network.URI as NU (URI)
import qualified Control.Concurrent as Con (Chan)
import qualified Data.Text as T (Text)

newtype JobId = JobId Int

data Link = Link JobId NU.URI

type LinkChannel = Con.Chan Link

data CrawlResult =
  CrawlResult
  { bodyText :: T.Text
  , links    :: [ NU.URI ]
  , workId   :: JobId
  }

type CrawlChannel = Con.Chan CrawlResult
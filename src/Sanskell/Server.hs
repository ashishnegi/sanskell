module Sanskell.Server where

import qualified Sanskell.Types as ST
import qualified Data.Text as T

addJob :: ST.Server -> String -> Either T.Text ST.JobId
addJob = undefined

jobResult :: ST.Server -> ST.JobId -> Maybe ST.JobResult
jobResult = undefined

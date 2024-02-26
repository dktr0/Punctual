module DateTime where

import Prelude (bottom,(*))
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..))

numberToDateTime :: Number -> DateTime
numberToDateTime x = case instant (Milliseconds (x*1000.0)) of
  Just i -> toDateTime i
  Nothing -> toDateTime bottom

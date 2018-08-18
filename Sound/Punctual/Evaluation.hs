module Sound.Punctual.Evaluation where

import Data.Time.Clock

import Sound.Punctual.Types
import Sound.Punctual.Phasor

data History = History {
  phasors :: [Phasor],
  expressions :: [Expression]
  }

emptyHistory :: History
emptyHistory = History {
  phasors = [],
  expressions = []
  }

type Evaluation = ([Expression],UTCTime)

evaluationNow :: [Expression] -> IO Evaluation
evaluationNow x = do
  now <- getCurrentTime
  return (x,now)

updateHistory :: History -> Evaluation -> History
updateHistory h e = History {
  phasors = [], -- placeholder
  expressions = fst e -- placeholder
  }

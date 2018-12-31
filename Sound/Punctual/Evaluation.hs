module Sound.Punctual.Evaluation where

import Data.Time.Clock

import Sound.Punctual.Types
import Sound.Punctual.Phasor

data PunctualState = PunctualState {
  expressions :: [Expression],
  startTime :: UTCTime
  }

emptyPunctualState :: AudioContext -> UTCTime -> PunctualState
emptyPunctualState t = PunctualState {
  expressions = [],
  startTime = t
  }

type Evaluation = ([Expression],UTCTime)

evaluationNow :: [Expression] -> IO Evaluation
evaluationNow x = do
  now <- getCurrentTime
  return (x,now)

updatePunctualState :: PunctualState -> Evaluation -> PunctualState
updatePunctualState s e = s {
  expressions = fst e,
  startTime = snd e
  }

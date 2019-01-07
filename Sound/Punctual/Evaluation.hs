module Sound.Punctual.Evaluation where

import Data.Time

import Sound.Punctual.Types
import Sound.Punctual.Phasor

data PunctualState = PunctualState {
  expressions :: [Expression],
  startTime :: UTCTime
  }

emptyPunctualState :: UTCTime -> PunctualState
emptyPunctualState t = PunctualState {
  expressions = [],
  startTime = t
  }

type Evaluation = ([Expression],UTCTime)

updatePunctualState :: PunctualState -> Evaluation -> PunctualState
updatePunctualState s e = s {
  expressions = fst e,
  startTime = snd e
  }

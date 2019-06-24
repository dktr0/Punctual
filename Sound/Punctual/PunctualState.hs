module Sound.Punctual.PunctualState where

import Data.Time

import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.Graph
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

updatePunctualState :: PunctualState -> Evaluation -> PunctualState
updatePunctualState s e = s {
  expressions = fst e,
  startTime = snd e
  }


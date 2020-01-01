module Sound.Punctual.PunctualState where

import Sound.Punctual.Types
import Sound.Punctual.Evaluation
import Sound.Punctual.Graph
import Sound.Punctual.Phasor

data PunctualState = PunctualState {
  program :: Program,
  startTime :: AudioTime
  }

emptyPunctualState :: AudioTime -> PunctualState
emptyPunctualState t = PunctualState {
  program = Program { directGLSL = Nothing, expressions = []},
  startTime = t
  }

updatePunctualState :: PunctualState -> Evaluation -> PunctualState
updatePunctualState s e = s {
  program = fst e,
  startTime = snd e
  }

module Sound.Punctual.Evaluation where

import Data.Time.Clock

import Sound.Punctual.Types
import Sound.Punctual.Phasor

data History = History {
  phasors :: [Phasor],
  expressions :: [Expression]
  }

type Evaluation = ([Expression],UTCTime)

updateHistory :: History -> Evaluation -> History
updateHistory h e = History {
  phasors = [], -- placeholder
  expressions = fst e -- placeholder
  }

-- pastGraph :: History -> Evaluation -> Graph? -- or whatever appropriate type from MusicW is
-- pastGraph h e =

-- futureGraph :: History -> Evaluation -> Graph?
-- futureGraph h e =

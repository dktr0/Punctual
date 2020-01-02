module Sound.Punctual.Evaluation where

import Data.Time
import Data.Text (Text)

import Sound.Punctual.Types
import Sound.Punctual.Graph

type Evaluation = (Program,AudioTime)

findGraphsForOutput :: Text -> Evaluation -> [Graph]
findGraphsForOutput outputName (p,_) = fmap (graph . definition) $ filter ((==NamedOutput outputName) . output) $ expressions p

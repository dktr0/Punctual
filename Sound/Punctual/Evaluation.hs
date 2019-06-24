module Sound.Punctual.Evaluation where

import Data.Time

import Sound.Punctual.Types
import Sound.Punctual.Graph
import Sound.Punctual.Phasor

type Evaluation = ([Expression],UTCTime)

findGraphsForOutput :: String -> Evaluation -> [Graph]
findGraphsForOutput outputName (xs,t) = fmap (graph . definition) $ filter ((==NamedOutput outputName) . output) xs


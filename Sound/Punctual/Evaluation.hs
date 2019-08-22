module Sound.Punctual.Evaluation where

import Data.Time
import Data.Text (Text)
import Sound.MusicW.AudioContext (AudioTime)

import Sound.Punctual.Types
import Sound.Punctual.Graph

type Evaluation = ([Expression],AudioTime)

findGraphsForOutput :: Text -> Evaluation -> [Graph]
findGraphsForOutput outputName (xs,_) = fmap (graph . definition) $ filter ((==NamedOutput outputName) . output) xs

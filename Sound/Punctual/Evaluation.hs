module Sound.Punctual.Evaluation where

import Data.Time
import Data.Text (Text)

import Sound.Punctual.Types
import Sound.Punctual.Graph

type Evaluation = ([Expression],UTCTime)

findGraphsForOutput :: Text -> Evaluation -> [Graph]
findGraphsForOutput outputName (xs,_) = fmap (graph . definition) $ filter ((==NamedOutput outputName) . output) xs

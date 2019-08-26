module Sound.Punctual.Evaluation where

import Data.Text (Text)

import Sound.Punctual.AudioTime
import Sound.Punctual.Graph
import Sound.Punctual.Program

type Evaluation = (Program,AudioTime)

module Sound.Punctual.Program where

import Data.IntMap.Strict
import Data.Text (Text)

import Sound.Punctual.Action

data Program = Program {
  directGLSL :: Maybe Text,
  actions :: IntMap Action
}

emptyProgram :: Program
emptyProgram = Program {
  directGLSL = Nothing,
  actions = empty
}

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Program where

import Data.IntMap.Strict
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.AudioTime
import Sound.Punctual.Action

data Program = Program {
  directGLSL :: Maybe Text,
  actions :: IntMap Action,
  evalTime :: AudioTime
  } deriving (Show, Eq, Generic, NFData)

emptyProgram :: Program
emptyProgram = Program {
  directGLSL = Nothing,
  actions = empty,
  evalTime = 0
}

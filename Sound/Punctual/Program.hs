{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Program where

import Data.IntMap.Strict as IntMap
import Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.AudioTime
import Sound.Punctual.Action

data Program = Program {
  directGLSL :: Maybe Text,
  textureMap :: Map Text Int,
  programNeedsAudioInputAnalysis :: Bool,
  programNeedsAudioOutputAnalysis :: Bool,
  actions :: IntMap Action,
  evalTime :: AudioTime
  } deriving (Show, Eq, Generic, NFData)

emptyProgram :: Program
emptyProgram = Program {
  directGLSL = Nothing,
  textureMap = Map.empty,
  programNeedsAudioInputAnalysis = False,
  programNeedsAudioOutputAnalysis = False,
  actions = IntMap.empty,
  evalTime = 0
}

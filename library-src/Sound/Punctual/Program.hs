{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Program where

import Data.IntMap.Strict as IntMap
import Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Time

import Sound.Punctual.Action
import Sound.Punctual.Graph

data Program = Program {
  directGLSL :: Maybe Text,
  textureSet :: Set TextureRef,
  programNeedsAudioInputAnalysis :: Bool,
  programNeedsAudioOutputAnalysis :: Bool,
  actions :: IntMap Action,
  evalTime :: UTCTime
  } deriving (Show, Eq, Generic, NFData)

emptyProgram :: UTCTime -> Program
emptyProgram _t0 = Program {
  directGLSL = Nothing,
  textureSet = Set.empty,
  programNeedsAudioInputAnalysis = False,
  programNeedsAudioOutputAnalysis = False,
  actions = IntMap.empty,
  evalTime = _t0
}

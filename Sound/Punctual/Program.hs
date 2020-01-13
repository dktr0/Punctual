{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Program where

import Data.IntMap.Strict as IntMap
import Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.AudioTime
import Sound.Punctual.Action

data Program = Program {
  directGLSL :: Maybe Text,
  textureRefs :: Map Text Int,
  actions :: IntMap Action,
  evalTime :: AudioTime
  } deriving (Show, Eq, Generic, NFData)

emptyProgram :: Program
emptyProgram = Program {
  directGLSL = Nothing,
  textureRefs = Map.empty,
  actions = IntMap.empty,
  evalTime = 0
}

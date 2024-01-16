module Sound.Punctual.Program where

import Data.DateTime (DateTime)
import Data.Map as Map
import Data.List as List

import Action (Action)

type Program = {
  actions :: Map.Map Int Action,
  evalTime :: DateTime,
  info :: ProgramInfo
  }

type ProgramInfo = {
  needsAudioInputAnalysis :: Boolean,
  needsAudioOutputAnalysis :: Boolean,
  needsWebCam :: Boolean,
  imgURLs :: List.List String,
  vidURLs :: List.List String
  }

emptyProgram :: DateTime -> Program
emptyProgram et = {
  actions: Map.empty,
  evalTime: et,
  info: emptyProgramInfo
  }

emptyProgramInfo :: ProgramInfo
emptyProgramInfo = {
  needsAudioInputAnalysis: false,
  needsAudioOutputAnalysis: false,
  needsWebCam: false,
  imgURLs: List.Nil,
  vidURLs: List.Nil
  }

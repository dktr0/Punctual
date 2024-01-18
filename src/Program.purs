module Program where

import Data.DateTime (DateTime)
import Data.List (List(..))
import Data.Maybe (Maybe)

import Action (Action)

type Program = {
  actions :: List (Maybe Action),
  evalTime :: DateTime,
  info :: ProgramInfo
  }

type ProgramInfo = {
  needsAudioInputAnalysis :: Boolean,
  needsAudioOutputAnalysis :: Boolean,
  needsWebCam :: Boolean,
  imgURLs :: List String,
  vidURLs :: List String
  }

emptyProgram :: DateTime -> Program
emptyProgram et = {
  actions: Nil,
  evalTime: et,
  info: emptyProgramInfo
  }

emptyProgramInfo :: ProgramInfo
emptyProgramInfo = {
  needsAudioInputAnalysis: false,
  needsAudioOutputAnalysis: false,
  needsWebCam: false,
  imgURLs: Nil,
  vidURLs: Nil
  }

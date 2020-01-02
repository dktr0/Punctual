{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Output where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.Extent

data Output =
  NoOutput |
  Panned Extent | Splay |
  Red | Green | Blue | Alpha | RGB | HSV
  deriving (Show,Eq,Generic,NFData)

outputsAudio :: Output -> Bool
outputsAudio (Panned _) = True
outputsAudio Splay = True
outputsAudio _ = False

outputsWebGL :: Output -> Bool
outputsWebGL = not . outputsAudio

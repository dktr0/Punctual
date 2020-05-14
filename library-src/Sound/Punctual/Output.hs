{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Output where

import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.Extent

data Output =
  Panned Extent | Splay |
  Red | Green | Blue |
  Hue | Saturation | Value |
  RGB | HSV | Alpha | Fdbk
  deriving (Show,Eq,Generic,NFData)

outputsAudio :: [Output] -> Bool
outputsAudio [] = False
outputsAudio ((Panned _):_) = True
outputsAudio (Splay:_) = True
outputsAudio (_:xs) = outputsAudio xs

outputsWebGL :: [Output] -> Bool
outputsWebGL [] = False
outputsWebGL (Red:_) = True
outputsWebGL (Green:_) = True
outputsWebGL (Blue:_) = True
outputsWebGL (Hue:_) = True
outputsWebGL (Saturation:_) = True
outputsWebGL (Value:_) = True
outputsWebGL (RGB:_) = True
outputsWebGL (HSV:_) = True
outputsWebGL (Alpha:_) = True
outputsWebGL (Fdbk:_) = True
outputsWebGL (_:xs) = outputsWebGL xs

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Output where

import Data.Text (Text)
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
outputsAudio ((Panned _):xs) = True
outputsAudio (Splay:xs) = True
outputsAudio (_:xs) = outputsAudio xs

outputsWebGL :: [Output] -> Bool
outputsWebGL [] = False
outputsWebGL (Red:xs) = True
outputsWebGL (Green:xs) = True
outputsWebGL (Blue:xs) = True
outputsWebGL (Hue:xs) = True
outputsWebGL (Saturation:xs) = True
outputsWebGL (Value:xs) = True
outputsWebGL (RGB:xs) = True
outputsWebGL (HSV:xs) = True
outputsWebGL (Alpha:xs) = True
outputsWebGL (Fdbk:xs) = True
outputsWebGL (_:xs) = outputsWebGL xs

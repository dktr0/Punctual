module Output where

import Prelude (class Eq,class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Output =
  Audio | -- splayed over all available outputs, which might be different in number in different Estuary clients (for example)
  AOut Int Int | -- first argument is channel offset second argument is number of channels (stereo is a synonym for AOut 0 2)
  Blend |
  RGBA |
  Add |
  Mul |
  RGB

derive instance Eq Output
derive instance Generic Output _
instance Show Output where
  show = genericShow

isAudioOutput :: Output -> Boolean
isAudioOutput Audio = true
isAudioOutput (AOut _ _) = true
isAudioOutput _ = false

audioOutputChannels :: Int -> Output -> Int
audioOutputChannels maxChnls Audio = maxChnls
audioOutputChannels _ (AOut _ n) = n
audioOutputChannels _ _ = 0

audioOutputOffset :: Output -> Int
audioOutputOffset (AOut o _) = o
audioOutputOffset _ = 0

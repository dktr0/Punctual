{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.DefTime where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Time
import Data.Tempo

import Sound.Punctual.Duration

data DefTime = After Duration | Quant Rational Duration deriving (Show,Eq,Generic,NFData)

calculateT1 :: Tempo -> UTCTime -> DefTime -> UTCTime
calculateT1 _ evalTime (After (Seconds t)) = addUTCTime (realToFrac t) evalTime
calculateT1 tempo evalTime (After (Cycles t)) = addUTCTime (realToFrac (t/freq tempo)) evalTime
calculateT1 tempo evalTime (Quant n (Seconds t)) = countToTime tempo $ nextBeat n (t * freq tempo) $ timeToCount tempo evalTime
calculateT1 tempo evalTime (Quant n (Cycles t)) = countToTime tempo $ nextBeat n t $ timeToCount tempo evalTime

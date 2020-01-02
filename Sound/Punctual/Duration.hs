{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Duration where

import GHC.Generics (Generic)
import Control.DeepSeq

data Duration = Seconds Double | Cycles Double deriving (Show,Eq,Generic,NFData)

{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Output where

import GHC.Generics (Generic)
import Control.DeepSeq

data Output = NoOutput | PannedOutput Extent | NamedOutput Text deriving (Show,Eq,Generic,NFData)

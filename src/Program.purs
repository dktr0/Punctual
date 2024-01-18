module Program where

import Prelude (($),map)
import Data.DateTime (DateTime)
import Data.List (List,catMaybes)
import Data.Maybe (Maybe)
import Data.Foldable (foldMap)

import Action (Action)
import Signal (SignalInfo,signalInfo)

type Program = {
  actions :: List (Maybe Action),
  evalTime :: DateTime
  }

programInfo :: forall z. { actions :: List (Maybe Action) | z } -> SignalInfo
programInfo x = foldMap signalInfo $ map _.signal $ catMaybes x.actions

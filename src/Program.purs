module Program where

import Prelude (($),map,bind,pure)
import Data.DateTime (DateTime)
import Data.List (List(..),catMaybes)
import Data.Maybe (Maybe)
import Data.Foldable (foldMap,any)
import Effect
import Effect.Now (nowDateTime)

import Action (Action,actionHasVisualOutput)
import Signal (SignalInfo,signalInfo)

type Program = {
  actions :: List (Maybe Action),
  evalTime :: DateTime
  }

programInfo :: forall z. { actions :: List (Maybe Action) | z } -> SignalInfo
programInfo x = foldMap signalInfo $ map _.signal $ catMaybes x.actions

emptyProgram :: Effect Program
emptyProgram = do
  evalTime <- nowDateTime
  pure { actions: Nil, evalTime }
  
programHasVisualOutput :: Program -> Boolean
programHasVisualOutput p = any actionHasVisualOutput $ catMaybes p.actions


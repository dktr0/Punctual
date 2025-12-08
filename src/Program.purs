module Program where

import Prelude (bind, pure, show, ($), (+), (<>))
import Data.DateTime (DateTime)
import Data.List (List(..), catMaybes, filter, length)
import Data.Maybe (Maybe)
import Data.Foldable (foldMap,any)
import Effect
import Effect.Now (nowDateTime)

import Action (Action,actionHasVisualOutput,actionHasAudioOutput,actionSignalInfo)
import Signal (SignalInfo)

type Program = {
  actions :: List (Maybe Action),
  evalTime :: DateTime
  }

programSignalInfo :: Program -> SignalInfo
programSignalInfo x = foldMap actionSignalInfo $ catMaybes x.actions

emptyProgram :: Effect Program
emptyProgram = do
  evalTime <- nowDateTime
  pure { actions: Nil, evalTime }
  
programHasVisualOutput :: Program -> Boolean
programHasVisualOutput p = any actionHasVisualOutput $ catMaybes p.actions

programHasAudioOutput :: Program -> Boolean
programHasAudioOutput p = any actionHasAudioOutput $ catMaybes p.actions

showProgram :: Program -> String
showProgram p = showAudioActions <> showVisualActions <> showNoActions
  where
    actions = catMaybes p.actions
    audioActions = filter actionHasAudioOutput actions
    visualActions = filter actionHasVisualOutput actions
    showAudioActions = 
      case length audioActions of 
        0 -> ""
        _ -> " audio:\n" <> foldMap show audioActions <> "\n"
    showVisualActions =
      case length visualActions of
        0 -> ""
        _ -> " visual:\n" <> foldMap show visualActions <> "\n"
    showNoActions =
      case (length audioActions + length visualActions) of
        0 -> "(program has no actions)\n"
        _ -> ""

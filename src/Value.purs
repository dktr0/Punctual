module Value where

-- Value represents Signals and functions over Signals (ie. Signal -> Signal, Signal -> Signal -> Signal, etc)
-- The List of Strings are the names of any function arguments, which appear also in the Signal constructor Reference

import Data.Either (Either)
import Parsing (ParseError,Position)

import Signal

data Value = 
  ValueSignal Position Signal |
  ValueString Position String |
  ValueInt Position Int |
  ValueNumber Position Number |
  ValueFunction Position (Value -> Either ParseError Value)
  
valuePosition :: Value -> Position
valuePosition (ValueSignal p _) = p
valuePosition (ValueString p _) = p
valuePosition (ValueInt p _) = p
valuePosition (ValueNumber p _) = p
valuePosition (ValueFunction p _) = p


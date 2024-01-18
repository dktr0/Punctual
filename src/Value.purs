module Value where

-- Value represents Signals and functions over Signals (ie. Signal -> Signal, Signal -> Signal -> Signal, etc)
-- The List of Strings are the names of any function arguments, which appear also in the Signal constructor Reference

import Prelude ((<$>),pure,(>>=),($),(<<<),class Applicative,class Show,show,(<>))
import Data.Either (Either)
import Parsing (ParseError(..),Position)
import Control.Monad.Error.Class (class MonadThrow,throwError)
import Data.List (List)
import Data.Traversable (traverse)
import Data.Int (toNumber)

import Signal (Signal(..))
import Action (Action)
import Output (Output)

data Value =
  ValueSignal Position Signal |
  ValueString Position String |
  ValueInt Position Int |
  ValueNumber Position Number |
  ValueFunction Position (Value -> Either ParseError Value) |
  ValueOutput Position Output |
  ValueAction Position Action
  
instance Show Value where
  show (ValueSignal p x) = "ValueSignal (" <> show p <> ") (" <> show x <> ")"
  show (ValueString p x) = "ValueString (" <> show p <> ") (" <> show x <> ")"
  show (ValueInt p x) = "ValueInt (" <> show p <> ") (" <> show x <> ")"
  show (ValueNumber p x) = "ValueNumber (" <> show p <> ") (" <> show x <> ")"
  show (ValueFunction _ _) = "ValueFunction..."
  show (ValueOutput p x) = "ValueOutput (" <> show p <> ") " <> show x
  show (ValueAction p x) = "ValueAction (" <> show p <> ") (" <> show x <> ")"


valuePosition :: Value -> Position
valuePosition (ValueSignal p _) = p
valuePosition (ValueString p _) = p
valuePosition (ValueInt p _) = p
valuePosition (ValueNumber p _) = p
valuePosition (ValueFunction p _) = p
valuePosition (ValueOutput p _) = p
valuePosition (ValueAction p _) = p

-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listValueToValueSignal :: forall m. Applicative m => MonadThrow ParseError m => Position -> List Value -> m Value
listValueToValueSignal p xs = traverse valueToSignal xs >>= (pure <<< ValueSignal p <<< SignalList)

-- convert a Value to a ValueSignal
-- succeeds only if the provided value can be meaningfully cast to a Signal
valueToValueSignal :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Value
valueToValueSignal x = ValueSignal (valuePosition x) <$> valueToSignal x

-- convert a Value to a Signal
-- succeeds only if the provided value can be meaningfully cast to a Signal
valueToSignal :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Signal
valueToSignal (ValueSignal _ x) = pure x
valueToSignal (ValueInt _ x) = pure $ Constant $ toNumber x
valueToSignal (ValueNumber _ x) = pure $ Constant x
valueToSignal (ValueString p _) = throwError $ ParseError "expected Signal (found String)" p
valueToSignal (ValueFunction p _) = throwError $ ParseError "expected Signal (found Function)" p
valueToSignal (ValueOutput p _) = throwError $ ParseError "expected Signal (found Output)" p
valueToSignal (ValueAction p _) = throwError $ ParseError "expected Signal (found Action)" p

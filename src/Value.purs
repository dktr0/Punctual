module Value where

-- Value represents Signals and functions over Signals (ie. Signal -> Signal, Signal -> Signal -> Signal, etc)
-- The List of Strings are the names of any function arguments, which appear also in the Signal constructor Reference

import Prelude ((<$>),pure,(>>=),($),(<<<),class Applicative,class Show,show,(<>))
import Data.Either (Either)
import Parsing (ParseError(..),Position)
import Data.List (List,find)
import Data.Traversable (traverse)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Error.Class (class MonadThrow,throwError)
import Control.Monad.Except.Trans (ExceptT,runExceptT)
import Control.Monad.Reader (ReaderT,runReaderT)
import Data.Map as Map
import Effect.Aff (Aff)
import Data.Tuple (Tuple)
import Effect.Ref (Ref)
import Data.Map (Map)

import Signal (Signal(..))
import Action (Action,signalToAction)
import Output (Output)
import MultiMode (MultiMode)

type Library = Map.Map String Value

type LibraryCache = Ref (Map String Library)

type P a = StateT Library (ReaderT LibraryCache (ExceptT ParseError Aff)) a

runP :: forall a. LibraryCache -> Map.Map String Value -> P a -> Aff (Either ParseError (Tuple a Library))
runP libCache lib p = runExceptT $ runReaderT (runStateT p lib) libCache 


data Value =
  ValueSignal Position Signal |
  ValueString Position String |
  ValueInt Position Int |
  ValueNumber Position Number |
  ValueFunction Position (Value -> P Value) |
  ValueOutput Position Output |
  ValueAction Position Action |
  ValuePolymorphic Position (List Value) -- audio can be an Output or a Signal, blend can be an Output or a Function, etc
  

  
instance Show Value where
  show (ValueSignal p x) = "ValueSignal (" <> show p <> ") (" <> show x <> ")"
  show (ValueString p x) = "ValueString (" <> show p <> ") (" <> show x <> ")"
  show (ValueInt p x) = "ValueInt (" <> show p <> ") (" <> show x <> ")"
  show (ValueNumber p x) = "ValueNumber (" <> show p <> ") (" <> show x <> ")"
  show (ValueFunction _ _) = "ValueFunction..."
  show (ValueOutput p x) = "ValueOutput (" <> show p <> ") " <> show x
  show (ValueAction p x) = "ValueAction (" <> show p <> ") (" <> show x <> ")"
  show (ValuePolymorphic p xs) = "ValuePolymorphic (" <> show p <> ") (" <> show xs <> ")"
  
showValueType :: Value -> String
showValueType (ValueSignal _ _) = "Signal"
showValueType (ValueString _ _) = "String"
showValueType (ValueInt _ _) = "Int"
showValueType (ValueNumber _ _) = "Number"
showValueType (ValueFunction _ _) = "Function"
showValueType (ValueOutput _ _) = "Output"
showValueType (ValueAction _ _) = "Action"
showValueType (ValuePolymorphic _ _) = "polymorphic value"

valuePosition :: Value -> Position
valuePosition (ValueSignal p _) = p
valuePosition (ValueString p _) = p
valuePosition (ValueInt p _) = p
valuePosition (ValueNumber p _) = p
valuePosition (ValueFunction p _) = p
valuePosition (ValueOutput p _) = p
valuePosition (ValueAction p _) = p
valuePosition (ValuePolymorphic p _) = p

-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listValueToValueSignal :: forall m. Applicative m => MonadThrow ParseError m => Position -> MultiMode -> List Value -> m Value
listValueToValueSignal p mm xs = traverse valueToSignal xs >>= (pure <<< ValueSignal p <<< SignalList mm)

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
valueToSignal (ValuePolymorphic p vs) = do
  case find isSignal vs of
    Nothing -> throwError $ ParseError ("expected Signal, found polymorphic value") p
    Just v -> valueToSignal v
valueToSignal v = throwError $ ParseError ("expected Signal, found " <> showValueType v) (valuePosition v)
    
isSignal :: Value -> Boolean
isSignal (ValueSignal _ _) = true
isSignal (ValueInt _ _) = true
isSignal (ValueNumber _ _) = true
isSignal _ = false

valueToOutput :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Output
valueToOutput (ValueOutput _ x) = pure x
valueToOutput (ValuePolymorphic p vs) = do
  case find isOutput vs of
    Nothing -> throwError $ ParseError ("expected Output, found polymorphic value") p
    Just v -> valueToOutput v
valueToOutput v = throwError $ ParseError ("expected Output, found " <> showValueType v) (valuePosition v)

isOutput :: Value -> Boolean
isOutput (ValueOutput _ _) = true
isOutput _ = false

valueToFunction :: forall m. Applicative m => MonadThrow ParseError m => Value -> m (Value -> P Value)
valueToFunction (ValueFunction _ x) = pure x
valueToFunction (ValuePolymorphic p vs) = do
  case find isFunction vs of
    Nothing -> throwError $ ParseError ("expected Function, found polymorphic value") p
    Just v -> valueToFunction v
valueToFunction v = throwError $ ParseError ("expected Function, found " <> showValueType v) (valuePosition v)

isFunction :: Value -> Boolean
isFunction (ValueFunction _ _) = true
isFunction _ = false

valueToString :: forall m. Applicative m => MonadThrow ParseError m => Value -> m String
valueToString (ValueString _ x) = pure x
valueToString (ValuePolymorphic p vs) = do
  case find isString vs of
    Nothing -> throwError $ ParseError ("expected String, found polymorphic value") p
    Just v -> valueToString v
valueToString v = throwError $ ParseError ("expected String, found " <> showValueType v) (valuePosition v)

isString :: Value -> Boolean
isString (ValueString _ _) = true
isString _ = false


valueToNumber :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Number
valueToNumber (ValueNumber _ x) = pure x
valueToNumber (ValueInt _ x) = pure $ toNumber x
valueToNumber (ValuePolymorphic p vs) = do
  case find isNumber vs of
    Nothing -> throwError $ ParseError ("expected Number, found polymorphic value") p
    Just v -> valueToNumber v
valueToNumber v = throwError $ ParseError ("expected Number, found " <> showValueType v) (valuePosition v)

isNumber :: Value -> Boolean
isNumber (ValueNumber _ _) = true
isNumber (ValueInt _ _) = true
isNumber _ = false


valueToInt :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Int
valueToInt (ValueInt _ x) = pure x
valueToInt (ValuePolymorphic p vs) = do
  case find isInt vs of
    Nothing -> throwError $ ParseError ("expected Int, found polymorphic value") p
    Just v -> valueToInt v
valueToInt v = throwError $ ParseError ("expected Int, found " <> showValueType v) (valuePosition v)

isInt :: Value -> Boolean
isInt (ValueInt _ _) = true
isInt _ = false


valueToAction :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Action
valueToAction (ValueAction _ x) = pure x
valueToAction (ValueSignal _ x) = pure $ signalToAction x
valueToAction (ValueNumber _ x) = pure $ signalToAction $ Constant x
valueToAction (ValueInt _ x) = pure $ signalToAction $ Constant $ toNumber x
valueToAction (ValuePolymorphic p vs) = do
  case find isAction vs of
    Nothing -> throwError $ ParseError ("expected Action, found polymorphic value") p
    Just v -> valueToAction v
valueToAction v = throwError $ ParseError ("expected Action, found " <> showValueType v) (valuePosition v)

isAction :: Value -> Boolean
isAction (ValueAction _ _) = true
isAction (ValueSignal _ _) = true
isAction (ValueNumber _ _) = true
isAction (ValueInt _ _) = true
isAction _ = false


class FromValue a where
  fromValue :: forall m. Applicative m => MonadThrow ParseError m => Value -> m a
  
instance FromValue Signal where
  fromValue = valueToSignal
  
instance FromValue Output where
  fromValue = valueToOutput
  
instance FromValue String where
  fromValue = valueToString
  
instance FromValue Number where
  fromValue = valueToNumber
  
instance FromValue Int where
  fromValue = valueToInt
  

class ToValue a where
  toValue :: Position -> a -> Value

instance ToValue Signal where
  toValue = ValueSignal

instance ToValue Output where
  toValue = ValueOutput
  
instance ToValue String where
  toValue = ValueString

instance ToValue Number where
  toValue = ValueNumber
  
instance ToValue Int where
  toValue = ValueInt
  
  


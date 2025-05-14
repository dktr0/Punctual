module Value where

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
import Data.Either (Either(..))

import Signal (Signal(..))
import Action (Action,signalToAction)
import Output (Output)
import MultiMode (MultiMode)
import AST (Expression,expressionPosition)

type Library = Map.Map String Value

type LibraryCache = Ref (Map String Library)

type P a = StateT Library (ReaderT LibraryCache (ExceptT ParseError Aff)) a

runP :: forall a. LibraryCache -> Map.Map String Value -> P a -> Aff (Either ParseError (Tuple a Library))
runP libCache lib p = runExceptT $ runReaderT (runStateT p lib) libCache 


data Variant =
  Signal Signal |
  String String |
  Int Int |
  Number Number |
  Function (Value -> P Value) |
  Output Output |
  Action Action |
  SignalSignal_v (Signal -> Signal) |
  OutputOrSignalSignal Output (Signal -> Signal)

{-  
instance Show Variant where
  show (ValueSignal p x) = "ValueSignal (" <> show p <> ") (" <> show x <> ")"
  show (ValueString p x) = "ValueString (" <> show p <> ") (" <> show x <> ")"
  show (ValueInt p x) = "ValueInt (" <> show p <> ") (" <> show x <> ")"
  show (ValueNumber p x) = "ValueNumber (" <> show p <> ") (" <> show x <> ")"
  show (ValueFunction _ _) = "ValueFunction..."
  show (ValueOutput p x) = "ValueOutput (" <> show p <> ") " <> show x
  show (ValueAction p x) = "ValueAction (" <> show p <> ") (" <> show x <> ")"
  show (ValuePolymorphic p xs) = "ValuePolymorphic (" <> show p <> ") (" <> show xs <> ")"
  show (ValueSignalSignal p e f) = "ValueSignalSignal (" <> show p <> ") (" <> show e <> ")"
-}
  
variantType :: Variant -> String
variantType (Signal _)  = "Signal"
variantType (String _) = "String"
variantType (Int _) = "Int"
variantType (Number _) = "Number"
variantType (Function _) = "Function"
variantType (Output _) = "Output"
variantType (Action _) = "Action"
variantType (SignalSignal_v _) = "Signal -> Signal"
variantType (OutputOrSignalSignal _ _) = "Output or (Signal -> Signal)"


{-
-- convert a Value to a ValueSignal
-- succeeds only if the provided value can be meaningfully cast to a Signal
valueToValueSignal :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Value
valueToValueSignal x = ValueSignal (valuePosition x) <$> valueToSignal x
-}


class FromVariant a where
  fromVariant :: Variant -> Either String a

instance FromVariant Signal where
  fromVariant (Signal x) = Right x
  fromVariant (Int x) = Right $ Constant $ toNumber x
  fromVariant (Number x) = Right $ Constant x
  fromVariant v = Left $ "expected Signal, found " <> variantType v

instance FromVariant Output where 
  fromVariant (Output x) = Right x
  fromVariant (OutputOrSignalSignal x _) = Right x
  fromVariant v = Left $ "expected Output, found " <> variantType v

variantToFunction :: Variant -> Either String (Value -> P Value)
variantToFunction (Function x) = Right x
variantToFunction v = Left $ "expected Function, found " <> variantType v

instance FromVariant String where
  fromVariant (String x) = Right x
  fromVariant v = Left $ "expected String, found " <> variantType v

instance FromVariant Number where
  fromVariant (Number x) = Right x
  fromVariant (Int x) = Right $ toNumber x
  fromVariant v = Left $ "expected Number, found " <> variantType v

instance FromVariant Int where
  fromVariant (Int x) = Right x
  fromVariant v = Left $ "expected Int, found " <> variantType v

variantToAction :: Variant -> Either String Action
variantToAction (Action x) = Right x
variantToAction (Signal x) = Right $ signalToAction x
variantToAction (Number x) = Right $ signalToAction $ Constant x
variantToAction (Int x) = Right $ signalToAction $ Constant $ toNumber x
variantToAction v = Left $ "expected Action, found " <> variantType v

variantToSignalSignal :: Variant -> Either String (Signal -> Signal)
variantToSignalSignal (SignalSignal_v x) = Right x
variantToSignalSignal (OutputOrSignalSignal _ x) = Right x
variantToSignalSignal v = Left $ "expected Signal -> Signal, found " <> variantType v


class ToVariant a where
  toVariant :: a -> Variant

instance ToVariant Signal where
  toVariant = Signal

instance ToVariant Output where
  toVariant = Output
  
instance ToVariant String where
  toVariant = String

instance ToVariant Number where
  toVariant = Number
  
instance ToVariant Int where
  toVariant = Int


type Value = { expression :: Expression, variant :: Variant }

toValue :: forall a. ToVariant a => Expression -> a -> Value  
toValue e a = { expression: e, variant: toVariant a }

fromValue :: forall a. FromVariant a => Value -> Either ParseError a
fromValue v = 
  case fromVariant v.variant of
    Left err -> Left $ ParseError err (expressionPosition v.expression)
    Right x -> Right x

valueToFunction :: Value -> Either ParseError (Value -> P Value)
valueToFunction v = 
  case variantToFunction v.variant of
    Left err -> Left $ ParseError err (expressionPosition v.expression)
    Right x -> Right x

valueToSignalSignal :: Value -> Either ParseError (Signal -> Signal)
valueToSignalSignal v = 
  case variantToSignalSignal v.variant of
    Left err -> Left $ ParseError err (expressionPosition v.expression)
    Right x -> Right x


-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listValueToValueSignal :: forall m. Applicative m => MonadThrow ParseError m => Expression -> MultiMode -> List Value -> m Value
listValueToValueSignal e mm xs = traverse fromValue xs >>= (pure <<< toValue e <<< SignalList mm)

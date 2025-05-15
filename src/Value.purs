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

import Signal (Signal(..),SignalSignal(..))
import Action (Action,signalToAction)
import Output (Output)
import MultiMode (MultiMode)
import AST (Expression(..),expressionPosition)

type Library = Map.Map String Value

type LibraryCache = Ref (Map String Library)

type P a = StateT Library (ReaderT LibraryCache (ExceptT ParseError Aff)) a

runP :: forall a. LibraryCache -> Map.Map String Value -> P a -> Aff (Either ParseError (Tuple a Library))
runP libCache lib p = runExceptT $ runReaderT (runStateT p lib) libCache 

newtype VariantFunction = VariantFunction (Variant -> Either String Variant)

data Variant =
  Signal Signal |
  String String |
  Int Int |
  Number Number |
  Output Output |
  Action Action |
  VariantFunction_v VariantFunction |
  SignalSignal_v SignalSignal |
  OutputOrSignalSignal Output SignalSignal

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
variantType (Output _) = "Output"
variantType (Action _) = "Action"
variantType (VariantFunction_v _) = "VariantFunction"
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

instance FromVariant VariantFunction where
  fromVariant (VariantFunction_v x) = Right x
  fromVariant (SignalSignal_v x) = Right $ signalSignalToVariantFunction x
  fromVariant (OutputOrSignalSignal _ x) = Right $ signalSignalToVariantFunction x
  fromVariant v = Left $ "expected VariantFunction, found " <> variantType v

signalSignalToVariantFunction :: SignalSignal -> VariantFunction
signalSignalToVariantFunction (SignalSignal f) = VariantFunction $ \v ->
  case fromVariant v of
    Left err -> Left err
    Right x -> Right $ toVariant $ f.signalSignal x

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

instance FromVariant Action where
  fromVariant (Action x) = Right x
  fromVariant (Signal x) = Right $ signalToAction x
  fromVariant (Number x) = Right $ signalToAction $ Constant x
  fromVariant (Int x) = Right $ signalToAction $ Constant $ toNumber x
  fromVariant v = Left $ "expected Action, found " <> variantType v

instance FromVariant SignalSignal where
  fromVariant (SignalSignal_v x) = Right x
  fromVariant (OutputOrSignalSignal _ x) = Right x
  fromVariant v = Left $ "expected Signal -> Signal, found " <> variantType v


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

instance ToVariant VariantFunction where
  toVariant = VariantFunction_v

instance ToVariant SignalSignal where
  toVariant = SignalSignal_v

instance ToVariant Action where
  toVariant = Action


type Value = { expression :: Expression, variant :: Variant }

toValue :: forall a. ToVariant a => Expression -> a -> Value
toValue e a = { expression: e, variant: toVariant a }

fromValue :: forall a. FromVariant a => Value -> Either ParseError a
fromValue v = 
  case fromVariant v.variant of
    Left err -> Left $ ParseError err (expressionPosition v.expression)
    Right x -> Right x


variantFunction :: forall a b. FromVariant a => ToVariant b => (a -> b) -> Variant
variantFunction f = VariantFunction_v $ VariantFunction $ \v ->
  case fromVariant v of
    Left err -> Left err
    Right a -> Right $ toVariant $ f a 
             

-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listValueToValueSignal :: Expression -> MultiMode -> List Value -> Either ParseError Value
listValueToValueSignal e mm xs = traverse fromValue xs >>= (pure <<< toValue e <<< SignalList mm)

module Value where

import Prelude (class Applicative, bind, pure, ($), (<>),(>>=),(<<<))
import Parsing (ParseError(..),Position)
import Data.Int (toNumber)
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
import Data.List (List)
import Data.Traversable (traverse)

import MultiMode (MultiMode)
import Signal (Signal(..),SignalSignal(..))
import Action (Action,signalToAction)
import Output (Output)
import AST (Expression(..),expressionPosition)

type Library = Map.Map String Variant

type LibraryCache = Ref (Map String Library)

type P a = StateT Library (ReaderT LibraryCache (ExceptT ParseError Aff)) a

runP :: forall a. LibraryCache -> Library -> P a -> Aff (Either ParseError (Tuple a Library))
runP libCache lib p = runExceptT $ runReaderT (runStateT p lib) libCache 

newtype VariantFunction = VariantFunction (Variant -> Either ParseError Variant)

data Variant =
  Signal Expression Signal |
  String Expression String |
  Int Expression Int |
  Number Expression Number |
  Output Expression Output |
  Action_v Expression Action |
  VariantFunction_v Expression VariantFunction |
  OutputOrVariantFunction Expression Output VariantFunction

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
variantType (Signal _ _)  = "Signal"
variantType (String _ _) = "String"
variantType (Int _ _) = "Int"
variantType (Number _ _) = "Number"
variantType (Output _ _) = "Output"
variantType (Action_v _ _) = "Action"
variantType (VariantFunction_v _ _) = "VariantFunction"
variantType (OutputOrVariantFunction _ _ _) = "Output or VariantFunction"

variantExpression :: Variant -> Expression
variantExpression (Signal e _)  = e
variantExpression (String e _)  = e
variantExpression (Int e _)  = e
variantExpression (Number e _)  = e
variantExpression (Output e _)  = e
variantExpression (Action_v e _)  = e
variantExpression (VariantFunction_v e _)  = e
variantExpression (OutputOrVariantFunction e _ _)  = e

variantPosition :: Variant -> Position
variantPosition (Signal e _)  = expressionPosition e
variantPosition (String e _)  = expressionPosition e
variantPosition (Int e _)  = expressionPosition e
variantPosition (Number e _)  = expressionPosition e
variantPosition (Output e _)  = expressionPosition e
variantPosition (Action_v e _)  = expressionPosition e
variantPosition (VariantFunction_v e _)  = expressionPosition e
variantPosition (OutputOrVariantFunction e _ _)  = expressionPosition e


{-
-- convert a Value to a ValueSignal
-- succeeds only if the provided value can be meaningfully cast to a Signal
valueToValueSignal :: forall m. Applicative m => MonadThrow ParseError m => Value -> m Value
valueToValueSignal x = ValueSignal (valuePosition x) <$> valueToSignal x
-}


class FromVariant a where
  fromVariant :: forall m. Applicative m => MonadThrow ParseError m => Variant -> m a

instance FromVariant Variant where
  fromVariant x = pure x

instance FromVariant Signal where
  fromVariant (Signal _ x) = pure x
  fromVariant (Int _ x) = pure $ Constant $ toNumber x
  fromVariant (Number _ x) = pure $ Constant x
  fromVariant v = throwError $ ParseError ("expected Signal, found " <> variantType v) (variantPosition v)

instance FromVariant Output where 
  fromVariant (Output _ x) = pure x
  fromVariant (OutputOrVariantFunction _ x _) = pure x
  fromVariant v = throwError $ ParseError ("expected Output, found " <> variantType v) (variantPosition v)

instance FromVariant VariantFunction where
  fromVariant (VariantFunction_v _ x) = pure x
  fromVariant (OutputOrVariantFunction _ _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected VariantFunction, found " <> variantType v) (variantPosition v)

instance FromVariant String where
  fromVariant (String _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected String, found " <> variantType v) (variantPosition v)

instance FromVariant Number where
  fromVariant (Number _ x) = pure x
  fromVariant (Int _ x) = pure $ toNumber x
  fromVariant v = throwError $ ParseError ("expected Number, found " <> variantType v) (variantPosition v)

instance FromVariant Int where
  fromVariant (Int _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected Int, found " <> variantType v) (variantPosition v)

instance FromVariant Action where
  fromVariant (Action_v _ x) = pure x
  fromVariant (Signal _ x) = pure $ signalToAction x
  fromVariant (Number _ x) = pure $ signalToAction $ Constant x
  fromVariant (Int _ x) = pure $ signalToAction $ Constant $ toNumber x
  fromVariant v = throwError $ ParseError ("expected Action, found " <> variantType v) (variantPosition v)

instance FromVariant SignalSignal where
  fromVariant (VariantFunction_v e (VariantFunction f)) = do
    let t = f $ Signal e $ Constant 0.0 -- note: e is not right but it doesn't matter because it has no impact on the overall result here
    case t of
      Left _ -> throwError $ ParseError "expected Signal -> Signal but function doesn't accept a Signal argument" (expressionPosition e)
      Right r -> do
        case (fromVariant r :: Either ParseError Signal) of
          Left _ -> throwError $ ParseError "expected Signal -> Signal but function doesn't produce a Signal result" (expressionPosition e)
          Right _ -> pure $ variantFunctionToSignalSignal e f
  fromVariant v = throwError $ ParseError ("expected Signal -> Signal, found " <> variantType v) (variantPosition v)

variantFunctionToSignalSignal :: Expression -> (Variant -> Either ParseError Variant) -> SignalSignal
variantFunctionToSignalSignal e f = SignalSignal { expression: e, signalSignal: f' }
  where f' x = case f (toVariant e x) of -- note: e is not right but it doesn't matter because it has no impact on the overall result here
                 Left _ -> Constant 0.0
                 Right v -> do
                   case (fromVariant v :: Either ParseError Signal) of
                     Left _ -> Constant 0.0
                     Right s -> s


class ToVariant a where
  toVariant :: Expression -> a -> Variant

instance ToVariant Variant where
  toVariant _ a = a

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

instance ToVariant Action where
  toVariant = Action_v


abVariantFunction :: forall a b. FromVariant a => ToVariant b => Expression -> (a -> b) -> VariantFunction
abVariantFunction e f = VariantFunction $ \v -> do
  a <- fromVariant v
  let e' = Application (expressionPosition e) e (variantExpression v)
  pure $ toVariant e' $ f a 

variantFunction :: forall a b. FromVariant a => ToVariant b => Expression -> (a -> b) -> Variant
variantFunction e f = VariantFunction_v e $ VariantFunction $ \v -> do
  a <- fromVariant v
  let e' = Application (expressionPosition e) e (variantExpression v)
  pure $ toVariant e' $ f a 

variantFunction2 :: forall a b c. FromVariant a => FromVariant b => ToVariant c => Expression -> (a -> b -> c) -> Variant
variantFunction2 e f = VariantFunction_v e $ VariantFunction $ \v -> do
  a <- fromVariant v
  let e' = Application (expressionPosition e) e (variantExpression v)
  pure $ variantFunction e' $ f a

variantFunction3 :: forall a b c d. FromVariant a => FromVariant b => FromVariant c => ToVariant d => Expression -> (a -> b -> c -> d) -> Variant
variantFunction3 e f = VariantFunction_v e $ VariantFunction $ \v -> do
  a <- fromVariant v
  let e' = Application (expressionPosition e) e (variantExpression v)
  pure $ variantFunction2 e' $ f a

application :: Variant -> Variant -> Either ParseError Variant
application f x = do
  let e = Application (variantPosition f) (variantExpression f) (variantExpression x)
  case f of
    {- (SignalSignal_v _ (SignalSignal f')) -> do
      x' <- fromVariant x
      pure $ toVariant e $ f'.signalSignal x' -}
    (VariantFunction_v _ (VariantFunction f')) -> do
      case f' x of
        Left err -> throwError err
        Right v -> pure v
    _ -> throwError $ ParseError ("left-side of application must be a function, found " <> variantType f) (variantPosition f) 

-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listVariantToVariantSignal :: Expression -> MultiMode -> List Variant -> P Variant
listVariantToVariantSignal e mm xs = traverse fromVariant xs >>= (pure <<< toVariant e <<< SignalList mm)

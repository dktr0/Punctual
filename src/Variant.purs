module Variant where

import Prelude (class Applicative, bind, pure, ($), (<$>), (<>))
import Parsing (ParseError(..),Position)
import Data.Int (toNumber)
import Control.Monad.Error.Class (class MonadThrow,throwError)
import Control.Monad.Except.Trans (ExceptT,runExceptT)
import Data.Map as Map
import Effect.Ref (Ref)
import Data.Map (Map)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Newtype (class Newtype,unwrap)

import Action (Action,matrixSignalToAction)
import Output (Output)
import AST (Expression(..),expressionPosition)
import Signal (Signal,fromInt,fromNumber)
import Matrix (Matrix,singleton)

type Library = Map.Map String Variant

type LibraryCache = Ref (Map String Library)

newtype VariantMatrix = VariantMatrix (Matrix Variant)

derive instance Newtype VariantMatrix _

type V a = Either ParseError a

newtype VariantFunction = VariantFunction (Variant -> V Variant)

data Variant = 
  String Expression String |
  Int Expression Int | -- can also be used as Number and Signal_v
  Number Expression Number | -- can also be used as Signal_v
  Signal_v Expression Signal |
  -- *** TODO, WORKING HERE: matrices need to be strictly typed so need a variant for each type of matrix
  VariantMatrix_v Expression VariantMatrix | -- note: any of the other Variant types can also be used as VariantMatrix_v
  Output Expression Output | 
  Action_v Expression Action |
  VariantFunction_v Expression VariantFunction |
  OutputOrVariantFunction Expression Output VariantFunction
 
variantType :: Variant -> String
variantType (String _ _) = "String"
variantType (Int _ _) = "Int"
variantType (Number _ _) = "Number"
variantType (Signal_v _ _)  = "Signal"
variantType (VariantMatrix_v _ _) = "VariantMatrix"
variantType (Output _ _) = "Output"
variantType (Action_v _ _) = "Action"
variantType (VariantFunction_v _ _) = "VariantFunction"
variantType (OutputOrVariantFunction _ _ _) = "Output or VariantFunction"

variantExpression :: Variant -> Expression
variantExpression (String e _)  = e
variantExpression (Int e _)  = e
variantExpression (Number e _)  = e
variantExpression (Signal_v e _)  = e
variantExpression (VariantMatrix_v e _)  = e
variantExpression (Output e _)  = e
variantExpression (Action_v e _)  = e
variantExpression (VariantFunction_v e _)  = e
variantExpression (OutputOrVariantFunction e _ _)  = e

variantPosition :: Variant -> Position
variantPosition (String e _)  = expressionPosition e
variantPosition (Int e _)  = expressionPosition e
variantPosition (Number e _)  = expressionPosition e
variantPosition (Signal_v e _)  = expressionPosition e
variantPosition (VariantMatrix_v e _)  = expressionPosition e
variantPosition (Output e _)  = expressionPosition e
variantPosition (Action_v e _)  = expressionPosition e
variantPosition (VariantFunction_v e _)  = expressionPosition e
variantPosition (OutputOrVariantFunction e _ _)  = expressionPosition e


class FromVariant a where
  fromVariant :: forall m. Applicative m => MonadThrow ParseError m => Variant -> m a

instance FromVariant Variant where
  fromVariant x = pure x

instance FromVariant String where
  fromVariant (String _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected String, found " <> variantType v) (variantPosition v)

instance FromVariant Int where
  fromVariant (Int _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected Int, found " <> variantType v) (variantPosition v)

instance FromVariant Number where
  fromVariant (Int _ x) = pure $ toNumber x
  fromVariant (Number _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected Number, found " <> variantType v) (variantPosition v)

instance FromVariant Signal where
  fromVariant (Int _ x) = pure $ fromInt x
  fromVariant (Number _ x) = pure $ fromNumber x
  fromVariant (Signal_v _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected Signal, found " <> variantType v) (variantPosition v)

instance FromVariant VariantMatrix where
  fromVariant (VariantMatrix_v _ x)  = pure x
  fromVariant v = pure $ VariantMatrix $ singleton v
  
instance FromVariant Output where 
  fromVariant (Output _ x) = pure x
  fromVariant (OutputOrVariantFunction _ x _) = pure x
  fromVariant v = throwError $ ParseError ("expected Output, found " <> variantType v) (variantPosition v)

instance FromVariant Action where
  fromVariant (Action_v _ x) = pure x
  fromVariant (Int _ x) = pure $ matrixSignalToAction $ singleton $ fromInt x
  fromVariant (Number _ x) = pure $ matrixSignalToAction $ singleton $ fromNumber x
  fromVariant (Signal_v _ x) = pure $ matrixSignalToAction $ singleton $ x
  fromVariant (VariantMatrix_v _ x) = matrixSignalToAction <$> traverse fromVariant (unwrap x)
  fromVariant v = throwError $ ParseError ("expected Action, found " <> variantType v) (variantPosition v)

instance FromVariant VariantFunction where
  fromVariant (VariantFunction_v _ x) = pure x
  fromVariant (OutputOrVariantFunction _ _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected VariantFunction, found " <> variantType v) (variantPosition v)


class ToVariant a where
  toVariant :: Expression -> a -> Variant

instance ToVariant Variant where
  toVariant _ a = a

instance ToVariant String where
  toVariant = String

instance ToVariant Int where
  toVariant = Int

instance ToVariant Number where
  toVariant = Number

instance ToVariant Signal where
  toVariant = Signal_v

instance ToVariant VariantMatrix where
  toVariant = VariantMatrix_v

instance ToVariant Output where
  toVariant = Output
  
instance ToVariant Action where
  toVariant = Action_v

instance ToVariant VariantFunction where
  toVariant = VariantFunction_v



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

application :: Variant -> Variant -> V Variant
application f x = do
  case f of
    (VariantFunction_v _ (VariantFunction f')) -> do
      case f' x of
        Left err -> throwError err
        Right v -> pure v
    _ -> throwError $ ParseError ("left-side of application must be a function, found " <> variantType f) (variantPosition f) 

{-
-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listVariantToVariantSignal :: forall m. Monad m => MonadThrow ParseError m => Expression -> MultiMode -> List Variant -> m Variant
listVariantToVariantSignal e mm xs = traverse fromVariant xs >>= (pure <<< toVariant e <<< SignalList mm)
-}
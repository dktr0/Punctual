module Variant where

import Prelude (class Applicative, pure, ($), (<>))
import Parsing (ParseError(..),Position)
import Data.Int (toNumber)
import Control.Monad.Error.Class (class MonadThrow,throwError)
import Data.Map as Map
import Effect.Ref (Ref)
import Data.Map (Map)
import Data.Either (Either)

-- import Action (Action,signalToAction)
import Output (Output)
import AST (Expression, expressionPosition)
import Signal (Signal, toSignalNumber)
import NumberMatrix (NumberMatrix, toNumberMatrix)

type Library = Map.Map String Variant

type LibraryCache = Ref (Map String Library)

{-
newtype VariantMatrix = VariantMatrix (Matrix Variant)

derive instance Newtype VariantMatrix _
-}

type V a = Either ParseError a

-- Expression argument is expression/location where application happens
-- so that that information can be included in the resulting Variant
newtype VariantFunction = VariantFunction (Expression -> Variant -> V Variant)

data Variant = 
  Int Expression Int |
  Number Expression Number |
  NumberSignal Expression (Signal Number) |
  NumberMatrix_v Expression NumberMatrix |
  String Expression String |
  Output Expression Output | 
  -- Action_v Expression Action |
  VariantFunction_v Expression VariantFunction |
  OutputOrVariantFunction Expression Output VariantFunction
 
variantType :: Variant -> String
variantType (Int _ _) = "Int"
variantType (Number _ _) = "Number"
variantType (NumberSignal _ _) = "NumberSignal"
variantType (NumberMatrix_v _ _) = "NumberMatrix"
variantType (String _ _) = "String"
variantType (Output _ _) = "Output"
-- variantType (Action_v _ _) = "Action"
variantType (VariantFunction_v _ _) = "VariantFunction"
variantType (OutputOrVariantFunction _ _ _) = "Output or VariantFunction"

isInt :: Variant -> Boolean
isInt (Int _ _) = true
isInt _ = false

isNumber :: Variant -> Boolean
isNumber (Number _ _) = true
isNumber x = isInt x

isNumberSignal :: Variant -> Boolean
isNumberSignal (NumberSignal _ _) = true
isNumberSignal x = isNumber x

isNumberMatrix :: Variant -> Boolean
isNumberMatrix (NumberMatrix_v _ _) = true
isNumberMatrix x = isNumberSignal x

isString :: Variant -> Boolean
isString (String _ _) = true
isString _ = false

isOutput :: Variant -> Boolean
isOutput (Output _ _) = true
isOutput (OutputOrVariantFunction _ _ _) = true
isOutput _ = false

{-
isAction :: Variant -> Boolean
isAction v = variantType v == "Action" || isSignal v
-}

isVariantFunction :: Variant -> Boolean
isVariantFunction (VariantFunction_v _ _) = true
isVariantFunction (OutputOrVariantFunction _ _ _) = true
isVariantFunction _ = false

variantExpression :: Variant -> Expression
variantExpression (Int e _)  = e
variantExpression (Number e _)  = e
variantExpression (NumberSignal e _) = e
variantExpression (NumberMatrix_v e _) = e
variantExpression (String e _)  = e
variantExpression (Output e _)  = e
-- variantExpression (Action_v e _)  = e
variantExpression (VariantFunction_v e _)  = e
variantExpression (OutputOrVariantFunction e _ _)  = e

variantPosition :: Variant -> Position
variantPosition (Int e _)  = expressionPosition e
variantPosition (Number e _)  = expressionPosition e
variantPosition (NumberSignal e _)  = expressionPosition e
variantPosition (NumberMatrix_v e _)  = expressionPosition e
variantPosition (String e _)  = expressionPosition e
variantPosition (Output e _)  = expressionPosition e
-- variantPosition (Action_v e _)  = expressionPosition e
variantPosition (VariantFunction_v e _)  = expressionPosition e
variantPosition (OutputOrVariantFunction e _ _)  = expressionPosition e


class FromVariant a where
  fromVariant :: forall m. Applicative m => MonadThrow ParseError m => Variant -> m a

instance FromVariant Variant where
  fromVariant x = pure x

instance FromVariant Int where
  fromVariant (Int _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected Int, found " <> variantType v) (variantPosition v)

instance FromVariant Number where
  fromVariant (Int _ x) = pure $ toNumber x
  fromVariant (Number _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected Number, found " <> variantType v) (variantPosition v)

instance FromVariant (Signal Number) where
  fromVariant (Int _ x) = pure $ toSignalNumber x
  fromVariant (Number _ x) = pure $ toSignalNumber x
  fromVariant (NumberSignal _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected NumberSignal, found " <> variantType v) (variantPosition v)

instance FromVariant NumberMatrix where
  fromVariant (Int _ x) = pure $ toNumberMatrix x
  fromVariant (Number _ x) = pure $ toNumberMatrix x
  fromVariant (NumberSignal _ x) = pure $ toNumberMatrix x
  fromVariant (NumberMatrix_v _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected NumberMatrix, found " <> variantType v) (variantPosition v)

instance FromVariant String where
  fromVariant (String _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected String, found " <> variantType v) (variantPosition v)

instance FromVariant Output where 
  fromVariant (Output _ x) = pure x
  fromVariant (OutputOrVariantFunction _ x _) = pure x
  fromVariant v = throwError $ ParseError ("expected Output, found " <> variantType v) (variantPosition v)

{-
instance FromVariant Action where
  fromVariant (Action_v _ x) = pure x
  fromVariant (Int _ x) = pure $ signalToAction $ fromInt x
  fromVariant (Number _ x) = pure $ signalToAction $ fromNumber x
  fromVariant (MatrixInt _ (IntMatrix x)) = pure $ signalToAction $ fromMatrixInt x
  fromVariant (MatrixNumber _ (NumberMatrix x)) = pure $ signalToAction $ fromMatrixNumber x
  fromVariant (Signal_v _ x) = pure $ signalToAction x
  fromVariant v = throwError $ ParseError ("expected Action, found " <> variantType v) (variantPosition v)
-}

instance FromVariant VariantFunction where
  fromVariant (VariantFunction_v _ x) = pure x
  fromVariant (OutputOrVariantFunction _ _ x) = pure x
  fromVariant v = throwError $ ParseError ("expected VariantFunction, found " <> variantType v) (variantPosition v)


class ToVariant a where
  toVariant :: Expression -> a -> Variant

instance ToVariant Variant where
  toVariant _ a = a

instance ToVariant Int where
  toVariant = Int

instance ToVariant Number where
  toVariant = Number

instance ToVariant (Signal Number) where
  toVariant = NumberSignal

instance ToVariant NumberMatrix where
  toVariant = NumberMatrix_v

instance ToVariant String where
  toVariant = String

instance ToVariant Output where
  toVariant = Output
  
{- instance ToVariant Action where
  toVariant = Action_v -}

instance ToVariant VariantFunction where
  toVariant = VariantFunction_v



{-
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

-}

{-
-- convert a list of Values to a single value that is a multi-channel Signal
-- succeeds only if each of the provided values can be meaningfully cast to a Signal
listVariantToVariantSignal :: forall m. Monad m => MonadThrow ParseError m => Expression -> MultiMode -> List Variant -> m Variant
listVariantToVariantSignal e mm xs = traverse fromVariant xs >>= (pure <<< toVariant e <<< SignalList mm)
-}

-- miscellaneous functions over Variant

{-
-- TODO: could potentially also define this for Action (would map fract into the Signal contained within the Action)
fract :: forall m. Monad m => MonadThrow ParseError m => Expression -> Variant -> m Variant
fract e v
  | isInt v = pure v
  | isNumber v = (Number.fract <$> fromVariant v) >>= Number e >>> pure
  | isMatrixInt v = pure v
  | isMatrixNumber v = (modify (map Number.fract) <$> fromVariant v) >>= MatrixNumber e >>> pure
  | isSignal v = (Signal.fract <$> fromVariant v) >>= Signal_v e >>> pure
  | otherwise = throwError $ ParseError ("fract cannot take an argument of type " <> variantType v) (variantPosition v)
-}
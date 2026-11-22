module Variant where

import Prelude (pure, ($), (<>))
import Data.Int (toNumber)
import Control.Monad.Error.Class (throwError)
import Data.Map as Map
import Effect.Ref (Ref)
import Data.Map (Map)
import Data.Either (Either)

-- import Action (Action,signalToAction)
import Output (Output)
import Signal (Signal, toSignalNumber)
import Matrix (Matrix, singleton)

type Library = Map.Map String Variant

type LibraryCache = Ref (Map String Library)

data Variant = 
  Int Int |
  Number Number |
  SignalNumber (Signal Number) |
  Matrix_ (Matrix Variant) |
  Function (Variant -> Either String Variant) |
  String String |
  Output Output | 
  -- Action_v Action |
  OutputOrFunction Output (Variant -> Either String Variant)
 
variantType :: Variant -> String
variantType (Int _) = "Int"
variantType (Number _) = "Number"
variantType (SignalNumber _) = "SignalNumber"
variantType (Matrix_ _) = "Matrix"
variantType (Function _) = "Function"
variantType (String _) = "String"
variantType (Output _) = "Output"
-- variantType (Action_v _) = "Action"
variantType (OutputOrFunction _ _) = "Output or Function"

class FromVariant a where
  fromVariant :: Variant -> Either String a

instance FromVariant Variant where
  fromVariant x = pure x

instance FromVariant Int where
  fromVariant (Int x) = pure x
  fromVariant v = throwError $ "expected Int, found " <> variantType v

instance FromVariant Number where
  fromVariant (Int x) = pure $ toNumber x
  fromVariant (Number x) = pure x
  fromVariant v = throwError $ "expected Number, found " <> variantType v

instance FromVariant (Signal Number) where
  fromVariant (Int x) = pure $ toSignalNumber x
  fromVariant (Number x) = pure $ toSignalNumber x
  fromVariant (SignalNumber x) = pure x
  fromVariant v = throwError $ "expected SignalNumber, found " <> variantType v

instance FromVariant (Matrix Variant) where
  fromVariant (Matrix_ x) = pure x
  fromVariant x = pure $ singleton x 

instance FromVariant String where
  fromVariant (String x) = pure x
  fromVariant v = throwError $ "expected String, found " <> variantType v

instance FromVariant Output where 
  fromVariant (Output x) = pure x
  fromVariant (OutputOrFunction x _) = pure x
  fromVariant v = throwError $ "expected Output, found " <> variantType v

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

class ToVariant a where
  toVariant :: a -> Variant

instance ToVariant Variant where
  toVariant a = a

instance ToVariant Int where
  toVariant = Int

instance ToVariant Number where
  toVariant = Number

instance ToVariant (Signal Number) where
  toVariant = SignalNumber

instance ToVariant (Matrix Variant) where
  toVariant = Matrix_

instance ToVariant String where
  toVariant = String

instance ToVariant Output where
  toVariant = Output
  
{- instance ToVariant Action where
  toVariant = Action_v -}




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
module Functions where

import Prelude (($),pure)
import Data.Int (toNumber)
import Data.Number (sin) as Number
import Parsing (ParseError(..))
import Control.Monad.Error.Class (throwError)

-- import Number as Number
import AST (Expression,expressionPosition)
import Variant (V, Variant(..), VariantFunction(..), toVariant, variantExpression)
import Signal as Signal
import NumberMatrix as NumberMatrix

sin :: Expression -> Variant
sin e = VariantFunction_v e $ VariantFunction _sin

_sin :: Expression -> Variant -> V Variant
_sin e (Int _ x) = pure $ toVariant e $ Number.sin $ toNumber x
_sin e (Number _ x) = pure $ toVariant e $ Number.sin x
_sin e (NumberSignal _ x) = pure $ toVariant e $ Signal.sin x
_sin e (NumberMatrix_v _ x) = pure $ toVariant e $ NumberMatrix.sin x
_sin _ v = throwError $ ParseError ("Int, Number, NumberSignal or NumberMatrix expected as 1st argument of sin") (expressionPosition $ variantExpression v)

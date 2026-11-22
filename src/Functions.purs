module Functions where

import Prelude (pure, ($), (<$>))
import Data.Int (toNumber)
import Data.Number (sin) as Number
import Control.Monad.Error.Class (throwError)
import Data.Either (Either)
import Data.Traversable (traverse)

import Variant (Variant(..), toVariant)
import Signal as Signal

sin :: Variant -> Either String Variant
sin (Int x) = pure $ toVariant $ Number.sin $ toNumber x
sin (Number x) = pure $ toVariant $ Number.sin x
sin (SignalNumber x) = pure $ toVariant $ Signal.sin x
sin (Matrix_ x) = Matrix_ <$> traverse sin x
sin _ = throwError $ "Int, Number, SignalNumber or Matrix expected as 1st argument of sin"

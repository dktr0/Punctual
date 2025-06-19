module NewSignal where

import W (W)
import G (G)
import Matrix (Matrix)
import AST (Expression)
import Parsing (ParseError)
import Data.Either (Either)
import Action (Action)
import Output (Output)

type Signal = { audio :: W (Either Number String), video :: G (Either Number String) }

type V a = Either ParseError a

newtype VariantFunction = VariantFunction (Variant -> V Variant)

data Variant = 
  String Expression String |
  Int Expression Int | -- can also be used as FloatConstant, FloatVariable, and Matrix_v
  Number Expression Number | -- can also be used as FloatVariable, and Matrix_v
  Signal Expression Signal | -- can also be used as Matrix_v
  Matrix_v Expression (Matrix Variant) |
  Output Expression Output | 
  Action_v Expression Action |
  VariantFunction_v Expression VariantFunction |
  OutputOrVariantFunction Expression Output VariantFunction

{-

Example:

-- defined in Float.purs
fx :: Signal
fx = { audio: pure (Left 0.0), video: ...computation in W monad that accesses environment... }

-- defined in Parser.purs
parseReserved :: Expression -> String -> L Variant
parseReserved e "fx" = pure $ Signal e fx

-- next step: build this out in separate modules

-}

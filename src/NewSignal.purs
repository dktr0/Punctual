module NewSignal where

import W (W)
import G (G)
import Matrix (Matrix)
import AST (Expression)
import Parsing (ParseError)
import Data.Either (Either)
import Action (Action)
import Output (Output)

-- Floats in Punctual might be constant/literal or variable/calculated
-- and they might fall on either side of those lines differently on the audio-side than the video-side
type Float = { audio :: W (Either Number String), video :: G (Either Number String) }

type V a = Either ParseError a

newtype VariantFunction = VariantFunction (Variant -> V Variant)

data Variant = 
  String Expression String |
  Int Expression Int | -- can also be used as Float and FloatMatrix
  Float Expression Float | -- can also be used as FloatMatrix
  FloatMatrix Expression (Matrix Float) |
  Output Expression Output |
  Action_v Expression Action |
  VariantFunction_v Expression VariantFunction |
  OutputOrVariantFunction Expression Output VariantFunction

{-
f x = x + 3 -- f :: VariantFunction 

rmap :: (FloatMatrix -> FloatMatrix) -> FloatMatrix -> FloatMatrix

rmap would check whether the provided first argument was a function
that could accept a FloatMatrix as its first argument and produce a floatmatrix
(possibly various special cases could be handled)
because FloatMatrix is inherently/directly dividable into rows/channels, this will work
and the former Signal type ceases to exist (essentially being replaced by FloatMatrix...)

data Signal =
  Constant Number |
  SignalList MultiMode (List Signal) |
  Append Signal Signal |
  Zip Signal Signal |
  Mono Signal |
  Rep Int Signal |
  Pi |
  Px | Py | Pxy | Aspect |


-- defined in Float.purs
fx :: Float 
fx = { audio: pure (Left 0.0), video: ...computation in W monad that accesses environment... }

-- defined in Parser.purs
parseReserved :: Expression -> String -> L Variant
parseReserved e "fx" = pure $ Float e fx

-- next step: build this out in separate modules, e.g. new Float module 

-}

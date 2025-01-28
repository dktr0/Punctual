module Number where

import Prelude (min,max,(>=),(&&),(<=),($),(/),(-),(*),(<),(<>),show,otherwise,class Semigroup,(+),(==),(>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (class Unfoldable1, unfoldr1, singleton)

divisionSafe :: Number -> Number -> Number
divisionSafe _ 0.0 = 0.0
divisionSafe x y = x/y

divisionUnsafe :: Number -> Number -> Number
divisionUnsafe x y = x/y

clip :: Number -> Number -> Number -> Number
clip e0 e1 x = clamp (min e0 e1) (max e0 e1) x

-- clamp is an unchecked clip (weird results if min and max edges are in wrong order)
clamp :: Number -> Number -> Number -> Number
clamp e0 e1 x = max e0 (min e1 x)

between :: Number -> Number -> Number -> Number
between e0 e1 x = if x >= (min e0 e1) && x <= (max e0 e1) then 1.0 else 0.0

smoothStep :: Number -> Number -> Number -> Number
smoothStep e0 e1 x = t * t * (3.0 - (2.0 * t))
  where t = clamp 0.0 1.0 $ divisionSafe (x - e0) (e1 - e0)

foreign import acosh :: Number -> Number
foreign import asinh :: Number -> Number
foreign import atanh :: Number -> Number
foreign import cbrt :: Number -> Number
foreign import cosh :: Number -> Number
foreign import log2 :: Number -> Number
foreign import log10 :: Number -> Number
foreign import sinh :: Number -> Number
foreign import tanh :: Number -> Number

-- wraps negative numbers in brackets, to be used where necessary
showNumber :: Number -> String
showNumber x
  | x < 0.0 = "(" <> show x <> ")"
  | otherwise = show x


fromThenTo :: forall f. Unfoldable1 f => Semigroup (f Number) => Number -> Number -> Number -> f Number
fromThenTo a b c
  -- in all non-generative cases the result is a single-channel matrix with just the starting value
  | a == b = singleton a
  | a == c = singleton a
  | (b > a) && (c < a) = singleton a
  | (b < a) && (c > a) = singleton a 
  -- in the generative case, the result is the starting value, the final value, and all values in between that are less than the final value
  | otherwise = singleton a <> unfoldr1 (fromThenToUnfolder c (b-a)) a <> singleton c

fromThenToUnfolder :: Number -> Number -> Number -> Tuple Number (Maybe Number)
fromThenToUnfolder finalValue delta prevValue = Tuple a maybeB
  where
    a = prevValue + delta
    maybeB = if (a + delta) >= finalValue then Nothing else Just a

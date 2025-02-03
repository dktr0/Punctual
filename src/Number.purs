module Number where

import Prelude (min,max,(>=),(&&),(<=),($),(/),(-),(*),(<),(<>),show,otherwise,class Semigroup,(+),(==),(>),(<$>),class Functor)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (class Unfoldable1, unfoldr1, singleton, range,iterateN)
import Data.Int (toNumber)

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


fromTo :: forall f. Unfoldable1 f => Functor f => Int -> Int -> f Number
fromTo x y = toNumber <$> range x (_fromToLimited x y)

_fromToLimited :: Int -> Int -> Int
_fromToLimited x y
  | y >= x = x + (min (y - x) (_fromToChannelLimit - 1))
  | otherwise = x - (min (x - y) (_fromToChannelLimit -1))

_fromToChannelLimit :: Int
_fromToChannelLimit = 64

fromThenTo :: forall f. Unfoldable1 f => Semigroup (f Number) => Number -> Number -> Number -> f Number
fromThenTo a b c
  -- in all non-generative cases the result is a single-channel matrix with just the starting value
  | a == b = singleton a
  | a == c = singleton a
  | b == c = singleton a <> singleton b
  | (b > a) && (c < a) = singleton a
  | (b < a) && (c > a) = singleton a 
  | _fromThenToChannels a b c > (toNumber _fromToChannelLimit) = fromThenTo a b (a+((b-a)*(toNumber _fromToChannelLimit - 1.0)))
  -- in the generative case, the result is the starting value, the final value, and all values in between that are less than the final value
  | a < c = singleton a <> unfoldr1 (_fromThenToUnfolderAscending c (b-a)) a <> singleton c
  | otherwise = singleton a <> unfoldr1 (_fromThenToUnfolderDescending c (b-a)) a <> singleton c

_fromThenToUnfolderAscending :: Number -> Number -> Number -> Tuple Number (Maybe Number)
_fromThenToUnfolderAscending finalValue delta prevValue = Tuple a maybeB
  where
    a = prevValue + delta
    maybeB = if (a + delta) >= (finalValue - (delta * 0.5)) then Nothing else Just a

_fromThenToUnfolderDescending :: Number -> Number -> Number -> Tuple Number (Maybe Number)
_fromThenToUnfolderDescending finalValue delta prevValue = Tuple a maybeB
  where
    a = prevValue + delta
    maybeB = if (a + delta) <= (finalValue - (delta * 0.5)) then Nothing else Just a

-- note: this is assumed to be correct only for the generative case
_fromThenToChannels :: Number -> Number -> Number -> Number
_fromThenToChannels a b c = ((c-a)/(b-a)) + 1.0
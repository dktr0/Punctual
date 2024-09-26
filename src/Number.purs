module Number where

import Prelude (min,max,(>=),(&&),(<=),($),(/),(-),(*),(<),(<>),show,otherwise)

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

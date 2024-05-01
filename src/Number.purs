module Number where

import Prelude (min,max,(>=),(&&),(<=),($),(/),(-),(*))

division :: Number -> Number -> Number
division _ 0.0 = 0.0
division x y = x/y

clip :: Number -> Number -> Number -> Number
clip e0 e1 x = uncheckedClip min' max' x
  where
    min' = min e0 e1
    max' = max e0 e1

uncheckedClip :: Number -> Number -> Number -> Number
uncheckedClip e0 e1 x = max e0 (min e1 x)

between :: Number -> Number -> Number -> Number
between e0 e1 x = if x >= min' && x <= max' then 1.0 else 0.0
  where
    min' = min e0 e1
    max' = max e0 e1

smoothStep :: Number -> Number -> Number -> Number
smoothStep e0 e1 x = t * t * (3.0 - (2.0 * t))
  where t = uncheckedClip 0.0 1.0 $ division (x - e0) (e1 - e0)

foreign import acosh :: Number -> Number
foreign import asinh :: Number -> Number
foreign import atanh :: Number -> Number
foreign import cbrt :: Number -> Number
foreign import cosh :: Number -> Number
foreign import log2 :: Number -> Number
foreign import log10 :: Number -> Number
foreign import sinh :: Number -> Number
foreign import tanh :: Number -> Number


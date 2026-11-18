module NumberMatrix where

import Prelude (class Eq, eq, ($), map)
import Data.Semiring (class Semiring, add, mul)
import Data.Semigroup (class Semigroup, append)
import Data.Newtype (class Newtype, modify)
import Data.Int (toNumber)

import MultiMode (MultiMode(..))
import Matrix (Matrix, singleton, combine)
import Signal (Signal, toSignalNumber)
import Signal (sin) as Signal


newtype NumberMatrix = NumberMatrix (Matrix (Signal Number))


-- creating from simpler types

class ToNumberMatrix a where
  toNumberMatrix :: a -> NumberMatrix

instance ToNumberMatrix Int where
  toNumberMatrix x = toNumberMatrix $ toNumber x

instance ToNumberMatrix Number where
  toNumberMatrix x = toNumberMatrix $ toSignalNumber x

instance ToNumberMatrix (Signal Number) where
  toNumberMatrix x = NumberMatrix $ singleton x

instance ToNumberMatrix NumberMatrix where
  toNumberMatrix x = x


-- instances of common classes

derive instance Newtype NumberMatrix _

instance Eq NumberMatrix where
  eq (NumberMatrix x) (NumberMatrix y) = eq x y

instance Semiring NumberMatrix where
  zero = toNumberMatrix 0.0
  one = toNumberMatrix 1.0
  add x y = addCombinatorial x y
  mul x y = mulCombinatorial x y

instance Semigroup NumberMatrix where
  append (NumberMatrix x) (NumberMatrix y) = NumberMatrix $ append x y
  
addCombinatorial :: NumberMatrix -> NumberMatrix -> NumberMatrix
addCombinatorial (NumberMatrix x) (NumberMatrix y) = NumberMatrix $ combine add Combinatorial x y

addPairwise :: NumberMatrix -> NumberMatrix -> NumberMatrix
addPairwise (NumberMatrix x) (NumberMatrix y) = NumberMatrix $ combine add Pairwise x y

mulCombinatorial :: NumberMatrix -> NumberMatrix -> NumberMatrix
mulCombinatorial (NumberMatrix x) (NumberMatrix y) = NumberMatrix $ combine mul Combinatorial x y

mulPairwise :: NumberMatrix -> NumberMatrix -> NumberMatrix
mulPairwise (NumberMatrix x) (NumberMatrix y) = NumberMatrix $ combine mul Pairwise x y


-- unary functions (e.g. trigonometry)

sin :: NumberMatrix -> NumberMatrix
sin x = modify (map Signal.sin) x


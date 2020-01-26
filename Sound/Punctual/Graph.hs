{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Graph where

import GHC.Generics (Generic)
import Control.DeepSeq

data Graph =
  LocalBinding Int |
  Constant Double |
  Multi [Graph] |
  Fx | Fy | Fxy |
  Px | Py |
  Lo | Mid | Hi | ILo | IMid | IHi |
  Mono Graph |
  Bipolar Graph |
  Unipolar Graph |
  Tex Int Graph |
  RgbHsv Graph |
  HsvRgb Graph |
  Sin Graph |
  Tri Graph |
  Saw Graph |
  Sqr Graph |
  Point Graph |
  Distance Graph |
  MidiCps Graph |
  CpsMidi Graph |
  DbAmp Graph |
  AmpDb Graph |
  Abs Graph |
  Sqrt Graph |
  Floor Graph |
  Fract Graph |
  Product Graph Graph |
  Sum Graph Graph |
  Max Graph Graph |
  Min Graph Graph |
  Division Graph Graph |
  GreaterThan Graph Graph |
  GreaterThanOrEqual Graph Graph |
  LessThan Graph Graph |
  LessThanOrEqual Graph Graph |
  Equal Graph Graph |
  NotEqual Graph Graph |
  Circle Graph Graph |
  Rect Graph Graph |
  Pow Graph Graph |
  Clip Graph Graph |
  Between Graph Graph |
  VLine Graph Graph |
  HLine Graph Graph |
  ILine Graph Graph Graph |
  Line Graph Graph Graph |
  LinLin Graph Graph Graph |
  LPF Graph Graph Graph |
  HPF Graph Graph Graph
  deriving (Show,Eq,Generic,NFData)

instance Num Graph where
  x + y = Sum x y
  x * y = Product x y
  negate x = Product x (Constant (-1))
  abs x = Abs x
  signum x = (GreaterThan x 0) + (LessThan x 0 * (-1))
  fromInteger x = Constant $ fromInteger x

instance Fractional Graph where
  x / y = Division x y
  fromRational x = Constant $ fromRational x

-- Miscellaneous functions over Graphs:

fb :: Graph -> Graph
fb xy = Tex 0 xy

modulatedRangeGraph :: Graph -> Graph -> Graph -> Graph
modulatedRangeGraph low high m = LinLin (Multi [-1,1]) (Multi [low,high]) m

(+-) :: Graph -> Graph -> Graph -> Graph
a +- b = modulatedRangeGraph (a - (a*b)) (a + (a*b))

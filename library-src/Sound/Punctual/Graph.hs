{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Graph where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Text

data Graph =
  LocalBinding Int |
  Constant Double |
  Multi [Graph] |
  Fx | Fy | Fxy |
  Px | Py |
  Lo | Mid | Hi | ILo | IMid | IHi |
  Cps | Time | Beat | EBeat | ETime |
  Rnd |
  FFT Graph | IFFT Graph |
  Mono Graph |
  Rep Int Graph |
  UnRep Int Graph |
  Bipolar Graph |
  Unipolar Graph |
  Fb Graph |
  Tex Text Graph |
  RgbHsv Graph | HsvRgb Graph |
  HsvH Graph | HsvS Graph | HsvV Graph | HsvR Graph | HsvG Graph | HsvB Graph |
  RgbH Graph | RgbS Graph | RgbV Graph | RgbR Graph | RgbG Graph | RgbB Graph |
  Sin Graph |
  Tri Graph |
  Saw Graph |
  Sqr Graph |
  LFTri Graph |
  LFSaw Graph |
  LFSqr Graph |
  Point Graph |
  Distance Graph |
  Prox Graph |
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
  Gate Graph Graph |
  Circle Graph Graph |
  Rect Graph Graph |
  Pow Graph Graph |
  Clip Graph Graph |
  Between Graph Graph |
  VLine Graph Graph |
  HLine Graph Graph |
  Step [Graph] Graph |
  IfThenElse Graph Graph Graph |
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

when :: Graph -> Graph -> Graph
when x y = IfThenElse x y 0

texhsv :: Text -> Graph -> Graph
texhsv t g = RgbHsv $ Tex t g

modulatedRangeGraph :: Graph -> Graph -> Graph -> Graph
modulatedRangeGraph low high m = LinLin (Multi [-1,1]) (Multi [low,high]) m

(+-) :: Graph -> Graph -> Graph -> Graph
a +- b = modulatedRangeGraph (a - (a*b)) (a + (a*b))

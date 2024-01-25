{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Graph where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Text

data MultiMode = Combinatorial | PairWise deriving (Show,Eq,NFData,Generic)

data TextureRef = ImgRef Text | VidRef Text deriving (Show,Eq,Ord,NFData,Generic)

data Graph =
  LocalBinding Int |
  Constant Double |
  Multi [Graph] |
  Append Graph Graph |
  Zip Graph Graph |
  Pi |
  Px | Py | Aspect |
  Fx | Fy | Fxy | -- cartesian coordinates of current fragment
  FRt | FR | FT | -- polar coordinates of current fragment
  Lo | Mid | Hi | ILo | IMid | IHi |
  Cps | Time | Beat | EBeat | ETime |
  Rnd |
  AudioIn |
  FFT Graph | IFFT Graph |
  Mono Graph |
  Rep Int Graph |
  UnRep Int Graph |
  Bipolar Graph |
  Unipolar Graph |
  Fb Graph |
  Tex TextureRef Graph | -- deprecated
  Img TextureRef |
  Vid TextureRef |
  Cam |
  Blend Graph |
  RgbHsv Graph | HsvRgb Graph |
  HsvH Graph | HsvS Graph | HsvV Graph | HsvR Graph | HsvG Graph | HsvB Graph |
  RgbH Graph | RgbS Graph | RgbV Graph | RgbR Graph | RgbG Graph | RgbB Graph |
  -- oscillators
  Osc Graph | Tri Graph | Saw Graph | Sqr Graph | LFTri Graph | LFSaw Graph | LFSqr Graph |
  -- unary Math functions based on (or emulating) JavaScript Math unary functions
  Abs Graph |
  Acos Graph |
  Acosh Graph |
  Asin Graph |
  Asinh Graph |
  Atan Graph |
  Atanh Graph |
  Cbrt Graph |
  Ceil Graph |
  Cos Graph |
  Cosh Graph |
  Exp Graph |
  Floor Graph |
  Log Graph |
  Log2 Graph |
  Log10 Graph |
  Round Graph |
  Sign Graph |
  Sin Graph |
  Sinh Graph |
  Sqrt Graph |
  Tan Graph |
  Tanh Graph |
  Trunc Graph |  
  -- other unary functions
  RtXy Graph | -- polar to cartesian conversion
  RtX Graph | -- x = r * cos theta
  RtY Graph | -- y = r * sin theta
  XyRt Graph | -- cartesian to polar conversion
  XyR Graph | -- r = sqrt (x^2 + y ^2)
  XyT Graph | -- theta = atan2(y,x)
  Point Graph |
  Distance Graph |
  Prox Graph |
  MidiCps Graph |
  CpsMidi Graph |
  DbAmp Graph |
  AmpDb Graph |
  Fract Graph |
  Sum MultiMode Graph Graph |
  Product MultiMode Graph Graph |
  Division MultiMode Graph Graph |
  Mod MultiMode Graph Graph |
  Pow MultiMode Graph Graph |
  Equal MultiMode Graph Graph |
  NotEqual MultiMode Graph Graph |
  GreaterThan MultiMode Graph Graph |
  GreaterThanOrEqual MultiMode Graph Graph |
  LessThan MultiMode Graph Graph |
  LessThanOrEqual MultiMode Graph Graph |
  Max MultiMode Graph Graph |
  Min MultiMode Graph Graph |
  Gate MultiMode Graph Graph |
  Circle MultiMode Graph Graph |
  Rect MultiMode Graph Graph |
  VLine MultiMode Graph Graph |
  HLine MultiMode Graph Graph |
  ILine MultiMode Graph Graph Graph |
  Line MultiMode Graph Graph Graph |
  Clip MultiMode Graph Graph |
  SmoothStep MultiMode Graph Graph |
  Between MultiMode Graph Graph |
  SetFx Graph Graph |
  SetFy Graph Graph |
  SetFxy Graph Graph |
  Zoom Graph Graph |
  Move Graph Graph |
  Tile Graph Graph |
  Spin Graph Graph |
  Delay MultiMode Double Graph Graph |
  LinLin MultiMode Graph Graph Graph |
  LPF MultiMode Graph Graph Graph |
  HPF MultiMode Graph Graph Graph |
  BPF MultiMode Graph Graph Graph |
  IfThenElse Graph Graph Graph |
  Step [Graph] Graph
  deriving (Show,Eq,Generic,NFData)

instance Num Graph where
  x + y = Sum Combinatorial x y
  x * y = Product Combinatorial x y
  negate x = Product Combinatorial x (Constant (-1))
  abs x = Abs x
  signum x = (GreaterThan Combinatorial x 0) + (LessThan Combinatorial x 0 * (-1))
  fromInteger x = Constant $ fromInteger x

instance Fractional Graph where
  x / y = Division Combinatorial x y
  fromRational x = Constant $ fromRational x

-- Miscellaneous functions over Graphs:

when :: Graph -> Graph -> Graph
when x y = IfThenElse x y 0

modulatedRangeLowHigh :: MultiMode -> Graph -> Graph -> Graph -> Graph
modulatedRangeLowHigh mm low high x = LinLin mm (Multi [-1,1]) (Multi [low,high]) x

modulatedRangePlusMinus :: MultiMode -> Graph -> Graph -> Graph -> Graph
modulatedRangePlusMinus mm a b = modulatedRangeLowHigh mm (a - (a*b)) (a + (a*b))

fit :: Graph -> Graph -> Graph
fit ar x = IfThenElse ((GreaterThanOrEqual Combinatorial) Aspect ar) (Zoom (Multi [ar/Aspect,1]) $ x) (Zoom (Multi [1,Aspect/ar]) $ x)

multi :: [[a]] -> [[a]]
multi [] = []
multi (xs:[]) = fmap pure xs
multi (xs:ys) = [ x:y | x <- xs, y <- multi ys ]

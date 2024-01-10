module Signal where

import Data.List (List)

data MultiMode = Combinatorial | PairWise -- deriving (Show,Eq,NFData,Generic)

data TextureRef = ImgRef String | VidRef String -- deriving (Show,Eq,Ord,NFData,Generic)

data Signal =
  LocalBinding Int |
  Constant Number |
  SignalList (List Signal) |
  Append Signal Signal |
  Zip Signal Signal |
  Pi |
  Px | Py | Aspect |
  Fx | Fy | Fxy | -- cartesian coordinates of current fragment
  FRt | FR | FT | -- polar coordinates of current fragment
  SetFx Signal Signal | SetFy Signal Signal | SetFxy Signal Signal |
  Zoom Signal Signal | Move Signal Signal | Tile Signal Signal | Spin Signal Signal |
  Lo | Mid | Hi | ILo | IMid | IHi |
  Cps | Time | Beat | EBeat | ETime |
  Rnd |
  AudioIn |
  FFT Signal | IFFT Signal |
  Mono Signal |
  Rep Int Signal |
  UnRep Int Signal |
  Bipolar Signal |
  Unipolar Signal |
  Fb Signal |
  Img TextureRef |
  Vid TextureRef |
  Cam |
  Blend Signal |
  RgbHsv Signal | HsvRgb Signal |
  HsvH Signal | HsvS Signal | HsvV Signal | HsvR Signal | HsvG Signal | HsvB Signal |
  RgbH Signal | RgbS Signal | RgbV Signal | RgbR Signal | RgbG Signal | RgbB Signal |
  -- oscillators
  Osc Signal | Tri Signal | Saw Signal | Sqr Signal | LFTri Signal | LFSaw Signal | LFSqr Signal |
  -- unary Math functions based on (or emulating) JavaScript Math unary functions
  Abs Signal |
  Acos Signal |
  Acosh Signal |
  Asin Signal |
  Asinh Signal |
  Atan Signal |
  Atanh Signal |
  Cbrt Signal |
  Ceil Signal |
  Cos Signal |
  Cosh Signal |
  Exp Signal |
  Floor Signal |
  Log Signal |
  Log2 Signal |
  Log10 Signal |
  Round Signal |
  Sign Signal |
  Sin Signal |
  Sinh Signal |
  Sqrt Signal |
  Tan Signal |
  Tanh Signal |
  Trunc Signal |  
  -- other unary functions
  RtXy Signal | -- polar to cartesian conversion
  RtX Signal | -- x = r * cos theta
  RtY Signal | -- y = r * sin theta
  XyRt Signal | -- cartesian to polar conversion
  XyR Signal | -- r = sqrt (x^2 + y ^2)
  XyT Signal | -- theta = atan2(y,x)
  Point Signal |
  Distance Signal |
  Prox Signal |
  MidiCps Signal |
  CpsMidi Signal |
  DbAmp Signal |
  AmpDb Signal |
  Fract Signal |
  Sum MultiMode Signal Signal |
  Product MultiMode Signal Signal |
  Division MultiMode Signal Signal |
  Mod MultiMode Signal Signal |
  Pow MultiMode Signal Signal |
  Equal MultiMode Signal Signal |
  NotEqual MultiMode Signal Signal |
  GreaterThan MultiMode Signal Signal |
  GreaterThanOrEqual MultiMode Signal Signal |
  LessThan MultiMode Signal Signal |
  LessThanOrEqual MultiMode Signal Signal |
  Max MultiMode Signal Signal |
  Min MultiMode Signal Signal |
  Gate MultiMode Signal Signal |
  Circle Signal Signal |
  Rect Signal Signal |
  Clip Signal Signal |
  Between Signal Signal |
  VLine Signal Signal |
  HLine Signal Signal |
  Step (List Signal) Signal |
  IfThenElse Signal Signal Signal |
  ILine Signal Signal Signal |
  Line Signal Signal Signal |
  LinLin Signal Signal Signal |
  LPF Signal Signal Signal | HPF Signal Signal Signal | BPF Signal Signal Signal |
  Delay Number Signal Signal
  -- deriving (Show,Eq,Generic,NFData)

{-
instance Num Signal where
  x + y = Sum Combinatorial x y
  x * y = Product Combinatorial x y
  negate x = Product Combinatorial x (Constant (-1))
  abs x = Abs x
  signum x = (GreaterThan Combinatorial x 0) + (LessThan Combinatorial x 0 * (-1))
  fromInteger x = Constant $ fromInteger x

instance Fractional Signal where
  x / y = Division Combinatorial x y
  fromRational x = Constant $ fromRational x

-- Miscellaneous functions over Signals:

when :: Signal -> Signal -> Signal
when x y = IfThenElse x y 0

modulatedRangeSignal :: Signal -> Signal -> Signal -> Signal
modulatedRangeSignal low high m = LinLin (Multi [-1,1]) (Multi [low,high]) m

(+-) :: Signal -> Signal -> Signal -> Signal
a +- b = modulatedRangeSignal (a - (a*b)) (a + (a*b))

fit :: Signal -> Signal -> Signal
fit ar x = IfThenElse ((GreaterThanOrEqual Combinatorial) Aspect ar) (Zoom (Multi [ar/Aspect,1]) $ x) (Zoom (Multi [1,Aspect/ar]) $ x)

multi :: [[a]] -> [[a]]
multi [] = []
multi (xs:[]) = fmap pure xs
multi (xs:ys) = [ x:y | x <- xs, y <- multi ys ]

-}


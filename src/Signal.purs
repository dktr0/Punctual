module Signal where

import Prelude (class Eq,class Show,negate,($))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..),(:))

data MultiMode = Combinatorial | Pairwise

derive instance Eq MultiMode
derive instance Generic MultiMode _
instance Show MultiMode where
  show = genericShow

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
  Img String |
  Vid String |
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
  Difference MultiMode Signal Signal |
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

derive instance Eq Signal
derive instance Generic Signal _
instance Show Signal where
  show x = genericShow x


-- Miscellaneous functions over Signals:

when :: Signal -> Signal -> Signal
when x y = IfThenElse x y (Constant 0.0)

modulatedRangeLowHigh :: Signal -> Signal -> Signal -> Signal
modulatedRangeLowHigh low high x = LinLin (SignalList $ Constant (-1.0):Constant 1.0:Nil) (SignalList $ low:high:Nil) x

modulatedRangePlusOrMinus :: Signal -> Signal -> Signal -> Signal
modulatedRangePlusOrMinus a b = modulatedRangeLowHigh low high 
  where
    low = Product Combinatorial a (Difference Pairwise (Constant 1.0) b)
    high = Product Combinatorial a (Sum Pairwise (Constant 1.0) b)
    
fit :: Signal -> Signal -> Signal
fit ar x = IfThenElse cond ifTrue ifFalse
  where
    cond = GreaterThanOrEqual Combinatorial Aspect ar
    ifTrue = Zoom (SignalList $ Division Pairwise ar Aspect : Constant 1.0 : Nil) x
    ifFalse = Zoom (SignalList $ Constant 1.0 : Division Pairwise Aspect ar : Nil) x

{-
-- was used in multiToGLSL in FragmentShader.hs, and expandMultis (for Multi) in PunctualW.hs
-- but it's not at all a function over Signal so perhaps it should be defined locally instead?
multi :: [[a]] -> [[a]]
multi [] = []
multi (xs:[]) = fmap pure xs
multi (xs:ys) = [ x:y | x <- xs, y <- multi ys ]
-}


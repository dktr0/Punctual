module Signal where

import Prelude (class Eq, class Show, mempty, negate, ($), (<>), pure, (*), map, (+), max, (/), (-))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..),(:))
import Data.List.NonEmpty (NonEmptyList,length,toList)
import Data.Foldable (foldMap)
import Data.Set (Set,singleton)
import Data.Monoid.Disj (Disj)
import Data.Semigroup.Foldable (foldl1)

import MultiMode
import Channels

data Signal =
  Constant Number |
  SignalList (NonEmptyList Signal) |
  Append Signal Signal |
  Zip Signal Signal |
  Mono Signal |
  Rep Int Signal |
  Pi |
  Px | Py | Pxy | Aspect |
  Fx | Fy | Fxy | -- cartesian coordinates of current fragment
  FRt | FR | FT | -- polar coordinates of current fragment
  Lo | Mid | Hi | ILo | IMid | IHi |
  Cps | Time | Beat | EBeat | ETime |
  Rnd |
  AudioIn |
  FFT |
  IFFT |
  Fb |
  Cam |
  Img String |
  Vid String |
  Bipolar Signal |
  Unipolar Signal |
  Blend Signal | Add Signal | Mul Signal |
  RgbHsv Signal | HsvRgb Signal | HsvR Signal | HsvG Signal | HsvB Signal | RgbH Signal | RgbS Signal | RgbV Signal | RgbR Signal | RgbG Signal | RgbB Signal |
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
  Dist Signal |
  Prox Signal |
  MidiCps Signal |
  CpsMidi Signal |
  DbAmp Signal |
  AmpDb Signal |
  Fract Signal |
  SetFx Signal Signal | SetFy Signal Signal | SetFxy Signal Signal |
  Zoom Signal Signal | ZoomXy Signal Signal | ZoomX Signal Signal | ZoomY Signal Signal |
  Move Signal Signal |
  Tile Signal Signal | TileXy Signal Signal | TileX Signal Signal | TileY Signal Signal |
  Spin Signal Signal |
  Early Signal Signal | Slow Signal Signal |
  Addition MultiMode Signal Signal |
  Difference MultiMode Signal Signal |
  Product MultiMode Signal Signal |
  Division MultiMode Signal Signal |
  Mod MultiMode Signal Signal |
  Pow MultiMode Signal Signal |
  Equal MultiMode Signal Signal |
  NotEqual MultiMode Signal Signal |
  GreaterThan MultiMode Signal Signal |
  GreaterThanEqual MultiMode Signal Signal |
  LessThan MultiMode Signal Signal |
  LessThanEqual MultiMode Signal Signal |
  Max MultiMode Signal Signal |
  Min MultiMode Signal Signal |
  Gate MultiMode Signal Signal |
  Circle MultiMode Signal Signal |
  Rect MultiMode Signal Signal |
  Clip MultiMode Signal Signal |
  Between MultiMode Signal Signal |
  SmoothStep MultiMode Signal Signal |
  VLine MultiMode Signal Signal |
  HLine MultiMode Signal Signal |
  Chain MultiMode Signal Signal |
  Lines MultiMode Signal Signal |
  ILines MultiMode Signal Signal |
  Mesh MultiMode Signal Signal |
  Seq Signal |
  Mix MultiMode Signal Signal Signal |
  ILine MultiMode Signal Signal Signal |
  Line MultiMode Signal Signal Signal |
  LinLin MultiMode Signal Signal Signal |
  LPF MultiMode Signal Signal Signal | HPF MultiMode Signal Signal Signal | BPF MultiMode Signal Signal Signal |
  Delay Number Signal Signal

derive instance Eq Signal
derive instance Generic Signal _
instance Show Signal where
  show x = genericShow x


-- Miscellaneous functions over Signals:

{-
when :: Signal -> Signal -> Signal
when x y = Mix (Constant 0.0) y x
-}

modulatedRangeLowHigh :: Signal -> Signal -> Signal -> Signal
modulatedRangeLowHigh low high x = LinLin Combinatorial (SignalList $ Constant (-1.0):Constant 1.0:Nil) (SignalList $ low:high:Nil) x

modulatedRangePlusMinus :: Signal -> Signal -> Signal -> Signal
modulatedRangePlusMinus a b = modulatedRangeLowHigh low high
  where
    low = Product Combinatorial a (Difference Pairwise (Constant 1.0) b)
    high = Product Combinatorial a (Addition Pairwise (Constant 1.0) b)

fit :: Signal -> Signal -> Signal
fit ar x = ZoomXy z x
  where
    cond = GreaterThanEqual Combinatorial Aspect ar
    ifTrue = SignalList $ Division Pairwise ar Aspect : Constant 1.0 : Nil
    ifFalse = SignalList $ Division Pairwise ar Aspect : Constant 1.0 : Nil
    z = Mix Combinatorial ifFalse ifTrue cond

{-
fit ar x = Mix Pairwise ifFalse ifTrue cond
  where
    cond = GreaterThanEqual Combinatorial Aspect ar
    ifTrue = Zoom (SignalList $ Division Pairwise ar Aspect : Constant 1.0 : Nil) x
    ifFalse = Zoom (c) x
-}

fast :: Signal -> Signal -> Signal
fast x = Slow (Division Pairwise (Constant 1.0) x)

late :: Signal -> Signal -> Signal
late x = Early (Division Pairwise (Constant 1.0) x)

type SignalInfo = {
  webcam :: Disj Boolean,
  fft :: Disj Boolean,
  lo :: Disj Boolean,
  mid :: Disj Boolean,
  hi :: Disj Boolean,
  ifft :: Disj Boolean,
  ilo :: Disj Boolean,
  imid :: Disj Boolean,
  ihi :: Disj Boolean,
  imgURLs :: Set String,
  vidURLs :: Set String
  }

emptySignalInfo :: SignalInfo
emptySignalInfo = {
  webcam: pure false,
  fft: pure false,
  lo: pure false,
  mid: pure false,
  hi: pure false,
  ifft: pure false,
  ilo: pure false,
  imid: pure false,
  ihi: pure false,
  imgURLs: mempty,
  vidURLs: mempty
  }

signalInfo :: Signal -> SignalInfo
signalInfo Cam = emptySignalInfo { webcam = pure true }
signalInfo ILo = emptySignalInfo { ilo = pure true }
signalInfo IMid = emptySignalInfo { imid = pure true }
signalInfo IHi = emptySignalInfo { ihi = pure true }
signalInfo IFFT = emptySignalInfo { ifft = pure true }
signalInfo Lo = emptySignalInfo { lo = pure true }
signalInfo Mid = emptySignalInfo { mid = pure true }
signalInfo Hi = emptySignalInfo { hi = pure true }
signalInfo FFT = emptySignalInfo { fft = pure true }
signalInfo (Img x) = emptySignalInfo { imgURLs = singleton x }
signalInfo (Vid x) = emptySignalInfo { vidURLs = singleton x }
signalInfo x = foldMap signalInfo $ subSignals x

-- given a Signal return the list of the component Signals it is dependent on
-- for example, Add x y is dependent on x and y, Bipolar x is dependent on x
subSignals :: Signal -> List Signal
subSignals (SignalList xs) = toList xs
subSignals (Append x y) = x:y:Nil
subSignals (Zip x y) = x:y:Nil
subSignals (Mono x) = x:Nil
subSignals (Rep _ x) = x:Nil
subSignals (Bipolar x) = x:Nil
subSignals (Unipolar x) = x:Nil
subSignals (Blend x) = x:Nil
subSignals (Add x) = x:Nil
subSignals (Mul x) = x:Nil
subSignals (RgbHsv x) = x:Nil
subSignals (HsvRgb x) = x:Nil
subSignals (HsvR x) = x:Nil
subSignals (HsvG x) = x:Nil
subSignals (HsvB x) = x:Nil
subSignals (RgbH x) = x:Nil
subSignals (RgbS x) = x:Nil
subSignals (RgbV x) = x:Nil
subSignals (RgbR x) = x:Nil
subSignals (RgbG x) = x:Nil
subSignals (RgbB x) = x:Nil
subSignals (Osc x) = x:Nil
subSignals (Tri x) = x:Nil
subSignals (Saw x) = x:Nil
subSignals (Sqr x) = x:Nil
subSignals (LFTri x) = x:Nil
subSignals (LFSaw x) = x:Nil
subSignals (LFSqr x) = x:Nil
subSignals (Abs x) = x:Nil
subSignals (Acos x) = x:Nil
subSignals (Acosh x) = x:Nil
subSignals (Asin x) = x:Nil
subSignals (Asinh x) = x:Nil
subSignals (Atan x) = x:Nil
subSignals (Atanh x) = x:Nil
subSignals (Cbrt x) = x:Nil
subSignals (Ceil x) = x:Nil
subSignals (Cos x) = x:Nil
subSignals (Cosh x) = x:Nil
subSignals (Exp x) = x:Nil
subSignals (Floor x) = x:Nil
subSignals (Log x) = x:Nil
subSignals (Log2 x) = x:Nil
subSignals (Log10 x) = x:Nil
subSignals (Round x) = x:Nil
subSignals (Sign x) = x:Nil
subSignals (Sin x) = x:Nil
subSignals (Sinh x) = x:Nil
subSignals (Sqrt x) = x:Nil
subSignals (Tan x) = x:Nil
subSignals (Tanh x) = x:Nil
subSignals (Trunc x) = x:Nil
subSignals (RtXy x) = x:Nil
subSignals (RtX x) = x:Nil
subSignals (RtY x) = x:Nil
subSignals (XyRt x) = x:Nil
subSignals (XyR x) = x:Nil
subSignals (XyT x) = x:Nil
subSignals (Point x) = x:Nil
subSignals (Dist x) = x:Nil
subSignals (Prox x) = x:Nil
subSignals (MidiCps x) = x:Nil
subSignals (CpsMidi x) = x:Nil
subSignals (DbAmp x) = x:Nil
subSignals (AmpDb x) = x:Nil
subSignals (Fract x) = x:Nil
subSignals (SetFx x y) = x:y:Nil
subSignals (SetFy x y) = x:y:Nil
subSignals (SetFxy x y) = x:y:Nil
subSignals (Zoom q z) = q:z:Nil
subSignals (ZoomX q z) = q:z:Nil
subSignals (ZoomY q z) = q:z:Nil
subSignals (Move x y) = x:y:Nil
subSignals (Tile x y) = x:y:Nil
subSignals (Spin x y) = x:y:Nil
subSignals (Early x y) = x:y:Nil
subSignals (Slow x y) = x:y:Nil
subSignals (Addition _ x y) = x:y:Nil
subSignals (Difference _ x y) = x:y:Nil
subSignals (Product _ x y) = x:y:Nil
subSignals (Division _ x y) = x:y:Nil
subSignals (Mod _ x y) = x:y:Nil
subSignals (Pow _ x y) = x:y:Nil
subSignals (Equal _ x y) = x:y:Nil
subSignals (NotEqual _ x y) = x:y:Nil
subSignals (GreaterThan _ x y) = x:y:Nil
subSignals (GreaterThanEqual _ x y) = x:y:Nil
subSignals (LessThan _ x y) = x:y:Nil
subSignals (LessThanEqual _ x y) = x:y:Nil
subSignals (Max _ x y) = x:y:Nil
subSignals (Min _ x y) = x:y:Nil
subSignals (Gate _ x y) = x:y:Nil
subSignals (Circle _ x y) = x:y:Nil
subSignals (Rect _ x y) = x:y:Nil
subSignals (Clip _ x y) = x:y:Nil
subSignals (Between _ x y) = x:y:Nil
subSignals (SmoothStep _ x y) = x:y:Nil
subSignals (VLine _ x y) = x:y:Nil
subSignals (HLine _ x y) = x:y:Nil
subSignals (Seq steps) = steps:Nil
subSignals (ILine _ x y z) = x:y:z:Nil
subSignals (Line _ x y z) = x:y:z:Nil
subSignals (LinLin _ x y z) = x:y:z:Nil
subSignals (Mix _ x y z) = x:y:z:Nil
subSignals (LPF _ x y z) = x:y:z:Nil
subSignals (HPF _ x y z) = x:y:z:Nil
subSignals (BPF _ x y z) = x:y:z:Nil
subSignals (Delay _ x y)  = x:y:Nil
subSignals _ = Nil

-- determine the dimensions of a Signal according to Punctual's multichannel matrix semantics
dimensions :: Signal -> { rows :: Int, columns :: Int }
dimensions (SignalList xs) = { rows: foldl1 (*) $ map channels xs, columns: length xs }
dimensions (Append x y) = { rows: 1, columns: channels x + channels y }
dimensions (Zip x y) = { rows: 1, columns: nPer 2 2 (channels x) + nPer 2 2 (channels y) }
dimensions (Rep n x) = { rows: n, columns: channels x }
dimensions Pxy = { rows: 1, columns: 2 }
dimensions Fxy = { rows: 1, columns: 2 }
dimensions FRt = { rows: 1, columns: 2 }
dimensions Fb = { rows: 1, columns: 3 }
dimensions Cam = { rows: 1, columns: 3 }
dimensions (Img _) = { rows: 1, columns: 3 }
dimensions (Vid _) = { rows: 1, columns: 3 }
dimensions (Bipolar x) = dimensions x
dimensions (Unipolar x) = dimensions x
dimensions (Blend _) = { rows: 1, columns: 4 }
dimensions (Add _) = { rows: 1, columns: 3 }
dimensions (Mul _) = { rows: 1, columns: 3 }
dimensions (RgbHsv x) = { rows: (dimensions x).rows, columns: nPer 3 3 (dimensions x).columns }
dimensions (HsvRgb x) = { rows: (dimensions x).rows, columns: nPer 3 3 (dimensions x).columns }
--...etc...
dimensions _ = { rows: 1, columns: 1 }

instance Channels Signal where
  channels x = y.rows * y.columns
    where y = dimensions x

{-
  channels (HsvR x) = nPer 1 3 (channels x)
  channels (HsvG x) = nPer 1 3 (channels x)
  channels (HsvB x) = nPer 1 3 (channels x)
  channels (RgbH x) = nPer 1 3 (channels x)
  channels (RgbS x) = nPer 1 3 (channels x)
  channels (RgbV x) = nPer 1 3 (channels x)
  channels (RgbR x) = nPer 1 3 (channels x)
  channels (RgbG x) = nPer 1 3 (channels x)
  channels (RgbB x) = nPer 1 3 (channels x)
  channels (Osc x) = channels x
  channels (Tri x) = channels x
  channels (Saw x) = channels x
  channels (Sqr x) = channels x
  channels (LFTri x) = channels x
  channels (LFSaw x) = channels x
  channels (LFSqr x) = channels x
  channels (Abs x) = channels x
  channels (Acos x) = channels x
  channels (Acosh x) = channels x
  channels (Asin x) = channels x
  channels (Asinh x) = channels x
  channels (Atan x) = channels x
  channels (Atanh x) = channels x
  channels (Cbrt x) = channels x
  channels (Ceil x) = channels x
  channels (Cos x) = channels x
  channels (Cosh x) = channels x
  channels (Exp x) = channels x
  channels (Floor x) = channels x
  channels (Log x) = channels x
  channels (Log2 x) = channels x
  channels (Log10 x) = channels x
  channels (Round x) = channels x
  channels (Sign x) = channels x
  channels (Sin x) = channels x
  channels (Sinh x) = channels x
  channels (Sqrt x) = channels x
  channels (Tan x) = channels x
  channels (Tanh x) = channels x
  channels (Trunc x) = channels x
  channels (RtXy x) = nPer 2 2 (channels x)
  channels (RtX x) = nPer 1 2 (channels x)
  channels (RtY x) = nPer 1 2 (channels x)
  channels (XyRt x) = nPer 2 2 (channels x)
  channels (XyR x) = nPer 1 2 (channels x)
  channels (XyT x) = nPer 1 2 (channels x)
  channels (Point x) = nPer 1 2 (channels x)
  channels (Dist x) = nPer 1 2 (channels x)
  channels (Prox x) = nPer 1 2 (channels x)
  channels (MidiCps x) = channels x
  channels (CpsMidi x) = channels x
  channels (DbAmp x) = channels x
  channels (AmpDb x) = channels x
  channels (Fract x) = channels x
  channels (SetFx x y) = channels x * channels y
  channels (SetFy x y) = channels x * channels y
  channels (SetFxy x y) = nPer 1 2 (channels x) * channels y
  channels (Zoom x y) = channels x * channels y
  channels (ZoomXy x y) = nPer 1 2 (channels x) * channels y
  channels (ZoomX x y) = channels x * channels y
  channels (ZoomY x y) = channels x * channels y
  channels (Move x y) = nPer 1 2 (channels x) * channels y
  channels (Tile x y) = channels x * channels y
  channels (TileXy x y) = nPer 1 2 (channels x) * channels y
  channels (TileX x y) = channels x * channels y
  channels (TileY x y) = channels x * channels y
  channels (Spin x y) = channels x * channels y
  channels (Early x y) = channels x * channels y
  channels (Slow x y) = channels x * channels y
  channels (Addition mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Difference mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Product mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Division mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Mod mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Pow mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Equal mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (NotEqual mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (GreaterThan mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (GreaterThanEqual mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (LessThan mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (LessThanEqual mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Max mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Min mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Gate mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Circle mm x y) = binaryFunctionChannels mm (nPer 1 2 $ channels x) (channels y)
  channels (Rect mm x y) = binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)
  channels (Clip mm x y) = binaryFunctionChannels mm (nPer 1 2 $ channels x) (channels y)
  channels (Between mm x y) = binaryFunctionChannels mm (nPer 1 2 $ channels x) (channels y)
  channels (SmoothStep mm x y) = binaryFunctionChannels mm (nPer 1 2 $ channels x) (channels y)
  channels (VLine mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (HLine mm x y) = binaryFunctionChannels mm (channels x) (channels y)
  channels (Chain mm x y) = binaryFunctionChannels (nPer 1 2 (channels x) ????
  channels (Lines mm x y) = binaryFunctionChannels mm (nPer 1 4 $ channels x) (channels y)
  channels (ILines mm x y) = binaryFunctionChannels mm (nPer 1 4 $ channels x) (channels y)
  channels (Mesh mm x y) = binaryFunctionChannels mm x' (channels y)
    where x = ...every pair from x...
  channels (Seq x) = ???
  channels (Mix mm x y z) = binaryFunctionChannels mm xy (channels z)
    where xy = max (channels x) (channels y)
  channels (ILine mm x y z) = binaryFunctionChannels mm (binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)) (channels z)
  channels (Line mm x y z) = binaryFunctionChannels mm (binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)) (channels z)
  channels (LinLin mm x y z) = binaryFunctionChannels mm (binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)) (channels z)
  channels (LPF mm x y z) = binaryFunctionChannels mm (binaryFunctionChannels mm (channels x) (channels y)) (channels z)
  channels (HPF mm x y z) = binaryFunctionChannels mm (binaryFunctionChannels mm (channels x) (channels y)) (channels z)
  channels (BPF mm x y z) = binaryFunctionChannels mm (binaryFunctionChannels mm (channels x) (channels y)) (channels z)
  channels (Delay _ x y) = channels x * channels y
  channels _ = 1
-}

nPer :: Int -> Int -> Int -> Int
nPer n per x = (((x - 1)/per)+1)*n

binaryFunctionChannels :: MultiMode -> Int -> Int -> Int
binaryFunctionChannels Combinatorial x y = x * y
binaryFunctionChannels Pairwise x y = max x y

module Signal where

import Prelude (class Eq, class Show, mempty, negate, ($), (<>), pure, (*), map, (+), max, (/), (-), (<=), otherwise)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..),(:))
import Data.List.NonEmpty (NonEmptyList,length,toList,fromList)
import Data.Foldable (foldMap)
import Data.Set (Set,singleton)
import Data.Monoid.Disj (Disj)
import Data.Semigroup.Foldable (foldl1)
import Data.Maybe (Maybe(..))

import MultiMode
import Channels

data Signal =
  Constant Number |
  SignalList MultiMode (List Signal) |
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
  AIn Int Int |
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

modulatedRangeLowHigh :: Signal -> Signal -> Signal -> Signal
modulatedRangeLowHigh low high x = LinLin Combinatorial inputRange outputRange x
  where
    inputRange = SignalList Combinatorial $ Constant (-1.0):Constant 1.0:Nil
    outputRange = SignalList Combinatorial $ low:high:Nil

modulatedRangePlusMinus :: Signal -> Signal -> Signal -> Signal
modulatedRangePlusMinus a b = modulatedRangeLowHigh low high
  where
    low = Difference Pairwise a b
    high = Addition Pairwise a b

fit :: Signal -> Signal -> Signal
fit ar x = ZoomXy z x
  where
    cond = GreaterThanEqual Combinatorial Aspect ar
    ifTrue = SignalList Combinatorial $ Division Pairwise ar Aspect : Constant 1.0 : Nil
    ifFalse = SignalList Combinatorial $ Division Pairwise ar Aspect : Constant 1.0 : Nil
    z = Mix Combinatorial ifFalse ifTrue cond

fast :: Signal -> Signal -> Signal
fast x = Slow (Division Pairwise (Constant 1.0) x)

late :: Signal -> Signal -> Signal
late x = Early (Difference Pairwise (Constant 0.0) x)

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
  ain :: Disj Boolean,
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
  ain: pure false,
  imgURLs: mempty,
  vidURLs: mempty
  }

signalInfo :: Signal -> SignalInfo
signalInfo Cam = emptySignalInfo { webcam = pure true }
signalInfo ILo = emptySignalInfo { ilo = pure true, ain = pure true }
signalInfo IMid = emptySignalInfo { imid = pure true, ain = pure true }
signalInfo IHi = emptySignalInfo { ihi = pure true, ain = pure true }
signalInfo IFFT = emptySignalInfo { ifft = pure true, ain = pure true }
signalInfo (AIn _ _) = emptySignalInfo { ain = pure true }
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
subSignals (SignalList _ xs) = xs
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
dimensions (SignalList Combinatorial xs) = 
  case fromList xs of
    Nothing -> { rows: 1, columns: 1 }
    Just xs' -> { rows: foldl1 (*) $ map channels xs', columns: length xs' }
dimensions (SignalList Pairwise xs) = 
  case fromList xs of
    Nothing -> { rows: 1, columns: 1 }
    Just xs' -> { rows: foldl1 max $ map channels xs', columns: length xs' }
dimensions (Append x y) = { rows: 1, columns: channels x + channels y }
dimensions (Zip x y) = { rows: 1, columns: nPer 2 2 (channels x) + nPer 2 2 (channels y) }
dimensions (Rep n x) = { rows: n, columns: channels x }
dimensions (AIn n _) = { rows: 1, columns: n }
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
dimensions (HsvR x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (HsvG x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (HsvB x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (RgbH x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (RgbS x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (RgbV x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (RgbR x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (RgbG x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (RgbB x) = { rows: (dimensions x).rows, columns: nPer 1 3 (dimensions x).columns }
dimensions (Osc x) = dimensions x
dimensions (Tri x) = dimensions x
dimensions (Saw x) = dimensions x
dimensions (Sqr x) = dimensions x
dimensions (LFTri x) = dimensions x
dimensions (LFSaw x) = dimensions x
dimensions (LFSqr x) = dimensions x
dimensions (Abs x) = dimensions x
dimensions (Acos x) = dimensions x
dimensions (Acosh x) = dimensions x
dimensions (Asin x) = dimensions x
dimensions (Asinh x) = dimensions x
dimensions (Atan x) = dimensions x
dimensions (Atanh x) = dimensions x
dimensions (Cbrt x) = dimensions x
dimensions (Ceil x) = dimensions x
dimensions (Cos x) = dimensions x
dimensions (Cosh x) = dimensions x
dimensions (Exp x) = dimensions x
dimensions (Floor x) = dimensions x
dimensions (Log x) = dimensions x
dimensions (Log2 x) = dimensions x
dimensions (Log10 x) = dimensions x
dimensions (Round x) = dimensions x
dimensions (Sign x) = dimensions x
dimensions (Sin x) = dimensions x
dimensions (Sinh x) = dimensions x
dimensions (Sqrt x) = dimensions x
dimensions (Tan x) = dimensions x
dimensions (Tanh x) = dimensions x
dimensions (Trunc x) = dimensions x
dimensions (MidiCps x) = dimensions x
dimensions (CpsMidi x) = dimensions x
dimensions (DbAmp x) = dimensions x
dimensions (AmpDb x) = dimensions x
dimensions (Fract x) = dimensions x
dimensions (RtXy x) = { rows: (dimensions x).rows, columns: nPer 2 2 (dimensions x).columns }
dimensions (RtX x) = { rows: (dimensions x).rows, columns: nPer 1 2 (dimensions x).columns }
dimensions (RtY x) = { rows: (dimensions x).rows, columns: nPer 1 2 (dimensions x).columns }
dimensions (XyRt x) = { rows: (dimensions x).rows, columns: nPer 2 2 (dimensions x).columns }
dimensions (XyR x) = { rows: (dimensions x).rows, columns: nPer 1 2 (dimensions x).columns }
dimensions (Point x) = { rows: (dimensions x).rows, columns: nPer 1 2 (dimensions x).columns }
dimensions (Dist x) = { rows: (dimensions x).rows, columns: nPer 1 2 (dimensions x).columns }
dimensions (Prox x) = { rows: (dimensions x).rows, columns: nPer 1 2 (dimensions x).columns }
dimensions (SetFx x y) = { rows: channels x, columns: channels y }
dimensions (SetFy x y) = { rows: channels x, columns: channels y }
dimensions (SetFxy x y) = { rows: nPer 1 2 (channels x), columns: channels y }
dimensions (Zoom x y) = { rows: channels x, columns: channels y }
dimensions (ZoomXy x y) = { rows: nPer 1 2 (channels x), columns: channels y }
dimensions (ZoomX x y) = { rows: channels x, columns: channels y }
dimensions (ZoomY x y) = { rows: channels x, columns: channels y }
dimensions (Move x y) = { rows: nPer 1 2 (channels x), columns: channels y }
dimensions (Tile x y) = { rows: channels x, columns: channels y }
dimensions (TileXy x y) = { rows: nPer 1 2 (channels x), columns: channels y }
dimensions (TileX x y) = { rows: channels x, columns: channels y }
dimensions (TileY x y) = { rows: channels x, columns: channels y }
dimensions (Spin x y) = { rows: channels x, columns: channels y }
dimensions (Early x y) = { rows: channels x, columns: channels y }
dimensions (Slow x y) = { rows: channels x, columns: channels y }
dimensions (Addition mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Difference mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Product mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Division mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Mod mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Pow mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Equal mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (NotEqual mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (GreaterThan mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (GreaterThanEqual mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (LessThan mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (LessThanEqual mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Max mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Min mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Gate mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Circle mm x y) = binaryFunctionDimensions mm (nPer 1 2 $ channels x) (channels y)
dimensions (Rect mm x y) = binaryFunctionDimensions mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)
dimensions (Clip mm x y) = binaryFunctionDimensions mm (nPer 1 2 $ channels x) (channels y)
dimensions (Between mm x y) = binaryFunctionDimensions mm (nPer 1 2 $ channels x) (channels y)
dimensions (SmoothStep mm x y) = binaryFunctionDimensions mm (nPer 1 2 $ channels x) (channels y)
dimensions (VLine mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (HLine mm x y) = binaryFunctionDimensions mm (channels x) (channels y)
dimensions (Chain mm xy w) = binaryFunctionDimensions mm (chainChannels $ channels xy) (channels w)
dimensions (Lines mm x y) = binaryFunctionDimensions mm (nPer 1 4 $ channels x) (channels y)
dimensions (ILines mm x y) = binaryFunctionDimensions mm (nPer 1 4 $ channels x) (channels y)
dimensions (Mesh mm xy w) = binaryFunctionDimensions mm (meshChannels $ channels xy) (channels w)
dimensions (Seq x) = { rows: 1, columns: (dimensions x).rows }
dimensions (Mix mm x y z) = binaryFunctionDimensions mm (max (channels x) (channels y)) (channels z)
dimensions (ILine mm xy1 xy2 w) = binaryFunctionDimensions mm (binaryFunctionChannels mm (nPer 1 2 $ channels xy1) (nPer 1 2 $ channels xy2)) (channels w)
dimensions (Line mm xy1 xy2 w) = binaryFunctionDimensions mm (binaryFunctionChannels mm (nPer 1 2 $ channels xy1) (nPer 1 2 $ channels xy2)) (channels w)
dimensions (ILine mm xy1 xy2 w) = binaryFunctionDimensions mm (binaryFunctionChannels mm (nPer 1 2 $ channels xy1) (nPer 1 2 $ channels xy2)) (channels w)
dimensions (LinLin mm x y z) = binaryFunctionDimensions mm (binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)) (channels z)
dimensions (LPF mm x y z) = binaryFunctionDimensions mm (binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)) (channels z)
dimensions (HPF mm x y z) = binaryFunctionDimensions mm (binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)) (channels z)
dimensions (BPF mm x y z) = binaryFunctionDimensions mm (binaryFunctionChannels mm (nPer 1 2 $ channels x) (nPer 1 2 $ channels y)) (channels z)
dimensions (Delay _ x y) = { rows: channels x, columns: channels y }
dimensions _ = { rows: 1, columns: 1 }

instance Channels Signal where
  channels x = y.rows * y.columns
    where y = dimensions x

chainChannels :: Int -> Int
chainChannels x
  | x <= 4 = 1
  | otherwise = (nPer 1 2 x) - 1 

meshChannels :: Int -> Int
meshChannels x
  | x <= 4 = 1
  | otherwise = n * (n - 1) / 2
      where n = nPer 1 2 x

nPer :: Int -> Int -> Int -> Int
nPer n per x = (((x - 1)/per)+1)*n

binaryFunctionDimensions :: MultiMode -> Int -> Int -> { rows :: Int, columns :: Int }
binaryFunctionDimensions Combinatorial x 1 = { rows: 1, columns: x } 
binaryFunctionDimensions Combinatorial x y = { rows: x, columns: y }
binaryFunctionDimensions Pairwise x y = { rows: 1, columns: max x y }

binaryFunctionChannels :: MultiMode -> Int -> Int -> Int
binaryFunctionChannels Combinatorial x y = x * y 
binaryFunctionChannels Pairwise x y = max x y

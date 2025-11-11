module Signal where

import Prelude (class Show, mempty, pure, show, ($))
import Data.Set (Set)
import Data.Monoid.Disj (Disj)
import Data.Either (Either(..))
import Data.Int (toNumber)

import G (G)
import W (W)


newtype Signal = Signal { audio :: W (Either Number String), video :: G (Either Number String), info :: SignalInfo, toShow :: String }

instance Show Signal where
  show (Signal x) = x.toShow

audio :: Signal -> W (Either Number String)
audio (Signal x) = x.audio

video :: Signal -> G (Either Number String)
video (Signal x) = x.video

info :: Signal -> SignalInfo
info (Signal x) = x.info

fromInt :: Int -> Signal
fromInt x = fromNumber $ toNumber x

fromNumber :: Number -> Signal
fromNumber x = Signal { audio: pure $ Left x, video: pure $ Left x, info: emptySignalInfo, toShow: show x }


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
  vidURLs :: Set String,
  gdmIDs :: Set String
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
  vidURLs: mempty,
  gdmIDs: mempty
  }

{-
data Signal =
  Constant Number | -- this is replaced by just Number
  SignalList MultiMode (List Signal) | -- ???
  Append Signal Signal | -- append :: Matrix a -> Matrix a -> Matrix a  
  Zip Signal Signal | -- zip :: Matrix a -> Matrix a -> Matrix a
  Mono Signal | -- mono :: Matrix Signal -> Signal
  Rep Int Signal | -- rep :: Int -> Matrix a -> Matrix a
  Pi | -- pi :: Number
  Px | Py | Pxy | Aspect | -- px, py, aspect :: Signal; Pxy :: Matrix Signal
  Fx | Fy | Fxy | -- cartesian coordinates of current fragment -- fx, fy :: Signal, fxy :: Matrix Signal
  FRt | FR | FT | -- polar coordinates of current fragment -- fr, ft :: Signal, frt :: Matrix Signal
  Lo | Mid | Hi | ILo | IMid | IHi | -- :: Signal
  Cps | Time | Beat | EBeat | ETime | -- :: Signal
  Rnd | -- :: Signal
  AIn Int Int | -- :: Signal
  FFT | -- :: Signal
  IFFT | -- :: Signal
  Fb | -- :: Matrix Signal
  Cam | Cama |  -- :: Matrix Signal
  Img String | Imga String | -- :: Matrix Signal
  Vid String | Vida String | -- :: Matrix Signal
  Gdm String | Gdma String | -- :: Matrix Signal
  Rows Signal | -- :: Matrix a -> Int
  Cols Signal | -- :: Matrix a -> Int
  Chns Signal | -- :: Matrix a -> Int
  Flat Signal | -- :: Matrix a -> Matrix a
  Trsp Signal | -- :: Matrix a -> Matrix a
  Get Int Int Signal | -- :: Int -> Int -> Matrix a -> Matrix a
  Set MultiMode Int Signal Signal | -- set :: Int -> Matrix a -> Matrix a -> Matrix a; setp :: Int -> Matrix a -> Matrix a -> Matrix a
  Map SignalSignal Signal | -- CONTINUE HERE
  Rmap SignalSignal Signal |
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
  Spr MultiMode Signal Signal |
  Btw MultiMode Int Signal Signal |
  Pan MultiMode Int Signal Signal |
  Splay Int Signal |
  Seq Signal |
  Mix MultiMode Signal Signal Signal |
  ILine MultiMode Signal Signal Signal |
  Line MultiMode Signal Signal Signal |
  LinLin MultiMode Signal Signal Signal |
  LPF MultiMode Signal Signal Signal | HPF MultiMode Signal Signal Signal | BPF MultiMode Signal Signal Signal |
  Delay Number Signal Signal
-}

{-
derive instance Eq Signal
derive instance Generic Signal _
instance Show Signal where
  show x = showIndented 0 x
-}

{-
data SignalSignal = SignalSignal { signalSignal :: Signal -> Signal, expression :: Expression }

-- a function from Signal to Signal is equal to another one if they are defined by the same expression
instance Eq SignalSignal where
  eq (SignalSignal x) (SignalSignal y) = eq x.expression y.expression
-}

{-
indent :: Int -> String
indent n = fold (replicate n " " :: List String)
-}

{-
showIndented :: Int -> Signal -> String
showIndented i (Constant x) = indent i <> "Constant " <> show x <> "\n"
showIndented i (SignalList Combinatorial xs) = indent i <> "[\n" <> fold (map (showIndented (i+1)) xs) <> indent i <> "]\n"
showIndented i (SignalList Pairwise xs) = indent i <> "{\n" <> fold (map (showIndented (i+1)) xs) <> indent i <> "}\n"
showIndented i (Append x y) = indent i <> "Append\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Zip x y) = indent i <> "Zip\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Mono x) = indent i <> "Mono\n" <> showIndented (i+1) x
showIndented i (Rep n x) = indent i <> "Rep " <> show n <> "\n" <> showIndented (i+1) x
showIndented i Pi = indent i <> "Pi\n"
showIndented i Px = indent i <> "Px\n"
showIndented i Py = indent i <> "Py\n"
showIndented i Pxy = indent i <> "Pxy\n"
showIndented i Aspect = indent i <> "Aspect\n"
showIndented i Fx = indent i <> "Fx\n"
showIndented i Fy = indent i <> "Fy\n"
showIndented i Fxy = indent i <> "Fxy\n"
showIndented i FRt = indent i <> "FRt\n"
showIndented i FR = indent i <> "FR\n"
showIndented i FT = indent i <> "FT\n"
showIndented i Lo = indent i <> "Lo\n"
showIndented i Mid = indent i <> "Mid\n"
showIndented i Hi = indent i <> "Hi\n"
showIndented i ILo = indent i <> "ILo\n"
showIndented i IMid = indent i <> "IMid\n"
showIndented i IHi = indent i <> "IHi\n"
showIndented i Cps = indent i <> "Cps\n"
showIndented i Time = indent i <> "Time\n"
showIndented i Beat = indent i <> "Beat\n"
showIndented i EBeat = indent i <> "EBeat\n"
showIndented i ETime = indent i <> "ETime\n"
showIndented i Rnd = indent i <> "Rnd\n"
showIndented i (AIn nchnls offset) = indent i <> "AIn " <> show nchnls <> " " <> show offset <> "\n"
showIndented i FFT = indent i <> "FFT\n"
showIndented i IFFT = indent i <> "IFFT\n"
showIndented i Fb = indent i <> "Fb\n"
showIndented i Cam = indent i <> "Cam\n"
showIndented i Cama = indent i <> "Cama\n"
showIndented i (Img url) = indent i <> "Img " <> url <> "\n"
showIndented i (Vid url) = indent i <> "Vid " <> url <> "\n"
showIndented i (Imga url) = indent i <> "Imga " <> url <> "\n"
showIndented i (Vida url) = indent i <> "Vida " <> url <> "\n"
showIndented i (Gdm x) = indent i <> "Gdm " <> x <> "\n"
showIndented i (Gdma x) = indent i <> "Gdma " <> x <> "\n"
showIndented i (Rows x) = indent i <> "Rows\n" <> showIndented (i+1) x
showIndented i (Cols x) = indent i <> "Cols\n" <> showIndented (i+1) x
showIndented i (Chns x) = indent i <> "Chns\n" <> showIndented (i+1) x
showIndented i (Flat x) = indent i <> "Flat\n" <> showIndented (i+1) x
showIndented i (Trsp x) = indent i <> "Trsp\n" <> showIndented (i+1) x
showIndented i (Get m n x) = indent i <> "Get " <> show m <> " " <> show n <> "\n" <> showIndented (i+1) x
showIndented i (Set mm m n x) = indent i <> "Set " <> show mm <> " " <> show m <> " " <> show n <> "\n" <> showIndented (i+1) x
showIndented i (Map _ x) = indent i <> "Map (Signal->Signal)\n" <> showIndented (i+1) x
showIndented i (Rmap _ x) = indent i <> "Rmap (Signal->Signal)\n" <> showIndented (i+1) x
showIndented i (Bipolar x) = indent i <> "Bipolar\n" <> showIndented (i+1) x
showIndented i (Unipolar x) = indent i <> "Unipolar\n" <> showIndented (i+1) x
showIndented i (Blend x) = indent i <> "Blend\n" <> showIndented (i+1) x
showIndented i (Add x) = indent i <> "Add\n" <> showIndented (i+1) x
showIndented i (Mul x) = indent i <> "Mul\n" <> showIndented (i+1) x
showIndented i (RgbHsv x) = indent i <> "RgbHsv\n" <> showIndented (i+1) x
showIndented i (HsvRgb x) = indent i <> "HsvRgb\n" <> showIndented (i+1) x
showIndented i (HsvR x) = indent i <> "HsvR\n" <> showIndented (i+1) x
showIndented i (HsvG x) = indent i <> "HsvG\n" <> showIndented (i+1) x
showIndented i (HsvB x) = indent i <> "HsvB\n" <> showIndented (i+1) x
showIndented i (RgbH x) = indent i <> "RgbH\n" <> showIndented (i+1) x
showIndented i (RgbS x) = indent i <> "RgbS\n" <> showIndented (i+1) x
showIndented i (RgbV x) = indent i <> "RgbV\n" <> showIndented (i+1) x
showIndented i (RgbR x) = indent i <> "RgbR\n" <> showIndented (i+1) x
showIndented i (RgbG x) = indent i <> "RgbG\n" <> showIndented (i+1) x
showIndented i (RgbB x) = indent i <> "RgbB\n" <> showIndented (i+1) x
showIndented i (Osc x) = indent i <> "Osc\n" <> showIndented (i+1) x
showIndented i (Tri x) = indent i <> "Tri\n" <> showIndented (i+1) x
showIndented i (Saw x) = indent i <> "Saw\n" <> showIndented (i+1) x
showIndented i (Sqr x) = indent i <> "Sqr\n" <> showIndented (i+1) x
showIndented i (LFTri x) = indent i <> "LFTri\n" <> showIndented (i+1) x
showIndented i (LFSaw x) = indent i <> "LFSaw\n" <> showIndented (i+1) x
showIndented i (LFSqr x) = indent i <> "LFSqr\n" <> showIndented (i+1) x
showIndented i (Abs x) = indent i <> "Abs\n" <> showIndented (i+1) x
showIndented i (Acos x) = indent i <> "Acos\n" <> showIndented (i+1) x
showIndented i (Acosh x) = indent i <> "Acosh\n" <> showIndented (i+1) x
showIndented i (Asin x) = indent i <> "Asin\n" <> showIndented (i+1) x
showIndented i (Asinh x) = indent i <> "Asinh\n" <> showIndented (i+1) x
showIndented i (Atan x) = indent i <> "Atan\n" <> showIndented (i+1) x
showIndented i (Atanh x) = indent i <> "Atanh\n" <> showIndented (i+1) x
showIndented i (Cbrt x) = indent i <> "Cbrt\n" <> showIndented (i+1) x
showIndented i (Ceil x) = indent i <> "Ceil\n" <> showIndented (i+1) x
showIndented i (Cos x) = indent i <> "Cos\n" <> showIndented (i+1) x
showIndented i (Cosh x) = indent i <> "Cosh\n" <> showIndented (i+1) x
showIndented i (Exp x) = indent i <> "Exp\n" <> showIndented (i+1) x
showIndented i (Floor x) = indent i <> "Floor\n" <> showIndented (i+1) x
showIndented i (Log x) = indent i <> "Log\n" <> showIndented (i+1) x
showIndented i (Log2 x) = indent i <> "Log2\n" <> showIndented (i+1) x
showIndented i (Log10 x) = indent i <> "Log10\n" <> showIndented (i+1) x
showIndented i (Round x) = indent i <> "Round\n" <> showIndented (i+1) x
showIndented i (Sign x) = indent i <> "Sign\n" <> showIndented (i+1) x
showIndented i (Sin x) = indent i <> "Sin\n" <> showIndented (i+1) x
showIndented i (Sinh x) = indent i <> "Sinh\n" <> showIndented (i+1) x
showIndented i (Sqrt x) = indent i <> "Sqrt\n" <> showIndented (i+1) x
showIndented i (Tan x) = indent i <> "Tan\n" <> showIndented (i+1) x
showIndented i (Tanh x) = indent i <> "Tanh\n" <> showIndented (i+1) x
showIndented i (Trunc x) = indent i <> "Trunc\n" <> showIndented (i+1) x
showIndented i (RtXy x) = indent i <> "RtXy\n" <> showIndented (i+1) x
showIndented i (RtX x) = indent i <> "RtX\n" <> showIndented (i+1) x
showIndented i (RtY x) = indent i <> "RtY\n" <> showIndented (i+1) x
showIndented i (XyRt x) = indent i <> "XyRt\n" <> showIndented (i+1) x
showIndented i (XyR x) = indent i <> "XyR\n" <> showIndented (i+1) x
showIndented i (XyT x) = indent i <> "XyT\n" <> showIndented (i+1) x
showIndented i (Point x) = indent i <> "Point\n" <> showIndented (i+1) x
showIndented i (Dist x) = indent i <> "Dist\n" <> showIndented (i+1) x
showIndented i (Prox x) = indent i <> "Prox\n" <> showIndented (i+1) x
showIndented i (MidiCps x) = indent i <> "MidiCps\n" <> showIndented (i+1) x
showIndented i (CpsMidi x) = indent i <> "CpsMidi\n" <> showIndented (i+1) x
showIndented i (DbAmp x) = indent i <> "DbAmp\n" <> showIndented (i+1) x
showIndented i (AmpDb x) = indent i <> "AmpDb\n" <> showIndented (i+1) x
showIndented i (Fract x) = indent i <> "Fract\n" <> showIndented (i+1) x
showIndented i (SetFx x y) = indent i <> "SetFx\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (SetFy x y) = indent i <> "SetFy\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (SetFxy x y) = indent i <> "SetFxy\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Zoom x y) = indent i <> "Zoom\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (ZoomXy x y) = indent i <> "ZoomXy\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (ZoomX x y) = indent i <> "ZoomX\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (ZoomY x y) = indent i <> "ZoomY\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Move x y) = indent i <> "Move\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Tile x y) = indent i <> "Tile\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (TileXy x y) = indent i <> "TileXy\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (TileX x y) = indent i <> "TileX\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (TileY x y) = indent i <> "TileY\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Spin x y) = indent i <> "Spin\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Early x y) = indent i <> "Early\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Slow x y) = indent i <> "Slow\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Addition mm x y) = indent i <> "Addition " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Difference mm x y) = indent i <> "Difference " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Product mm x y) = indent i <> "Product " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Division mm x y) = indent i <> "Division " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Mod mm x y) = indent i <> "Mod " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Pow mm x y) = indent i <> "Pow " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Equal mm x y) = indent i <> "Equal " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (NotEqual mm x y) = indent i <> "NotEqual " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (GreaterThan mm x y) = indent i <> "GreaterThan " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (GreaterThanEqual mm x y) = indent i <> "GreaterThanEqual " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (LessThan mm x y) = indent i <> "LessThan " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (LessThanEqual mm x y) = indent i <> "LessThanEqual " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Max mm x y) = indent i <> "Max " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Min mm x y) = indent i <> "Min " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Gate mm x y) = indent i <> "Gate " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Circle mm x y) = indent i <> "Circle " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Rect mm x y) = indent i <> "Rect " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Clip mm x y) = indent i <> "Clip " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Between mm x y) = indent i <> "Between " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (SmoothStep mm x y) = indent i <> "SmoothStep " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (VLine mm x y) = indent i <> "VLine " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (HLine mm x y) = indent i <> "HLine " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Chain mm x y) = indent i <> "Chain " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Lines mm x y) = indent i <> "Lines " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (ILines mm x y) = indent i <> "ILines " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Mesh mm x y) = indent i <> "Mesh " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Spr mm x y) = indent i <> "Spr " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Btw mm n x y) = indent i <> "Btw " <> show mm <> " " <> show n <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Pan mm n x y) = indent i <> "Pan " <> show mm <> " " <> show n <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
showIndented i (Splay n x) = indent i <> "Splay " <> show n <> "\n" <> showIndented (i+1) x
showIndented i (Seq x) = indent i <> "Seq\n" <> showIndented (i+1) x
showIndented i (Mix mm x y z) = indent i <> "Mix " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y <> showIndented (i+1) z
showIndented i (ILine mm x y z) = indent i <> "ILine " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y <> showIndented (i+1) z
showIndented i (Line mm x y z) = indent i <> "Line" <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y <> showIndented (i+1) z
showIndented i (LinLin mm x y z) = indent i <> "LinLin " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y <> showIndented (i+1) z
showIndented i (LPF mm x y z) = indent i <> "LPF " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y <> showIndented (i+1) z
showIndented i (HPF mm x y z) = indent i <> "HPF " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y <> showIndented (i+1) z
showIndented i (BPF mm x y z) = indent i <> "BPF " <> show mm <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y <> showIndented (i+1) z
showIndented i (Delay maxTime x y) = indent i <> "Delay " <> show maxTime <> "\n" <> showIndented (i+1) x <> showIndented (i+1) y
-}

{-
-- Miscellaneous functions over Signals:

modulatedRangeLowHigh :: MultiMode -> Signal -> Signal -> Signal -> Signal
modulatedRangeLowHigh mm low high x = LinLin mm inputRange outputRange x
  where
    inputRange = SignalList Pairwise $ Constant (-1.0):Constant 1.0:Nil
    outputRange = SignalList mm $ low:high:Nil

modulatedRangePlusMinus :: Signal -> Signal -> Signal -> Signal
modulatedRangePlusMinus a b = modulatedRangeLowHigh Pairwise low high
  where
    low = Difference Pairwise a b
    high = Addition Pairwise a b

-}

{-

uni x -- converts any signal x to unipolar, based on its nominal range (former unipolar deprecated)
bi x -- converts any signal x to bipolar, based on its nominal range (former bipolar deprecated)
range r x -- converts any signal x to the range r, based on its nominal range (has pairwise and unclipped variants, operators: <~ and ~>
runi r x -- converts signal x to unipolar, on the assumption that it ranges according to r (has pairwise and unclipped variants)
rbi r x -- converts signal x to bipolar, on the assumption that it ranges according to r (has pairwise and unclipped variants)

examples:

range [10,10000] $ lfsaw 0.1 -- produces an output that goes from 10 to 10000 at 0.1 Hz
[10,10000] <~ lfsaw 0.1 -- same as above (operator equivalent)
lfsaw 0.1 ~> [10,10000] -- same as above (operator equivalent with flipped arguments)
[10,10000] <~ hi;  -- produces an output that goes from 10 to 10000 driven by unipolar input from audio analysis
fx ~> [10,10000]; -- produces an output that goes from 10 to 10000 as fx goes from -1 to 1 (left to right across screen)

range [10,10000] $ runi [3.0,117.0] x -- the linlin replacement, assume x ranges 3 to 117 even if it doesn't, scale that 10 to 10000
[10,10000] <~ (runi [3.0,117.0] x) -- same as above

but how is nominal range calculated for multichannel signals? is it the widest range over all channels or does it exist for each channel?

4 full-range oscillators...
osc [110,220,330,440] -- range is clearly [-1,1]

4 LFOs...
range [0,1] $ [osc 1, osc 2 / 2, osc 3 / 3, osc 4 / 4]
all 4 LFOS are rescaled in such a way that they stay within [0,1] 

okay, so multichannel signals have the ranges that result from combining their component signals (the lowest minimum, the highest maximum)

while this is a nice idea, it runs afoul of cases like (Mono x :: Signal), where to know the range you have to mappend the ranges of all channels of the argument x

to return to later...

-}

{-
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
-}

{-
signalInfo :: Signal -> SignalInfo
signalInfo Cam = emptySignalInfo { webcam = pure true }
signalInfo Cama = emptySignalInfo { webcam = pure true }
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
signalInfo (Imga x) = emptySignalInfo { imgURLs = singleton x }
signalInfo (Vid x) = emptySignalInfo { vidURLs = singleton x }
signalInfo (Vida x) = emptySignalInfo { vidURLs = singleton x }
signalInfo (Gdm x) = emptySignalInfo { gdmIDs = singleton x }
signalInfo (Gdma x) = emptySignalInfo { gdmIDs = singleton x }
signalInfo x = foldMap signalInfo $ subSignals x
-}

{-
-- given a Signal return the list of the component Signals it is dependent on
-- for example, Add x y is dependent on x and y, Bipolar x is dependent on x
subSignals :: Signal -> List Signal
subSignals (Constant _) = Nil
subSignals (SignalList _ xs) = xs
subSignals (Append x y) = x:y:Nil
subSignals (Zip x y) = x:y:Nil
subSignals (Mono x) = x:Nil
subSignals (Rep _ x) = x:Nil
subSignals Pi = Nil
subSignals Px = Nil
subSignals Py = Nil
subSignals Pxy = Nil
subSignals Aspect = Nil
subSignals Fx = Nil
subSignals Fy = Nil
subSignals Fxy = Nil
subSignals FRt = Nil
subSignals FR = Nil
subSignals FT = Nil
subSignals Lo = Nil
subSignals Mid = Nil
subSignals Hi = Nil
subSignals ILo = Nil
subSignals IMid = Nil
subSignals IHi = Nil
subSignals Cps = Nil
subSignals Time = Nil
subSignals Beat = Nil
subSignals EBeat = Nil
subSignals ETime = Nil
subSignals Rnd = Nil
subSignals (AIn _ _) = Nil
subSignals FFT = Nil
subSignals IFFT = Nil
subSignals Fb = Nil
subSignals Cam = Nil
subSignals Cama = Nil
subSignals (Img _) = Nil
subSignals (Imga _) = Nil
subSignals (Vid _) = Nil
subSignals (Vida _) = Nil
subSignals (Gdm _) = Nil
subSignals (Gdma _) = Nil
subSignals (Rows x) = x:Nil
subSignals (Cols x) = x:Nil
subSignals (Chns x) = x:Nil
subSignals (Flat x) = x:Nil
subSignals (Trsp x) = x:Nil
subSignals (Get _ _ x) = x:Nil
subSignals (Set _ _ x y) = x:y:Nil
subSignals (Map _ x) = x:Nil
subSignals (Rmap _ x) = x:Nil
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
subSignals (ZoomXy q z) = q:z:Nil
subSignals (ZoomX q z) = q:z:Nil
subSignals (ZoomY q z) = q:z:Nil
subSignals (Move x y) = x:y:Nil
subSignals (Tile x y) = x:y:Nil
subSignals (TileXy x y) = x:y:Nil
subSignals (TileX x y) = x:y:Nil
subSignals (TileY x y) = x:y:Nil
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
subSignals (Chain _ x y) = x:y:Nil
subSignals (Lines _ x y) = x:y:Nil
subSignals (ILines _ x y) = x:y:Nil
subSignals (Mesh _ x y) = x:y:Nil
subSignals (Spr _ x y) = x:y:Nil
subSignals (Btw _ _ x y) = x:y:Nil
subSignals (Pan _ _ x y) = x:y:Nil
subSignals (Splay _ x) = x:Nil
subSignals (Seq x) = x:Nil
subSignals (ILine _ x y z) = x:y:z:Nil
subSignals (Line _ x y z) = x:y:z:Nil
subSignals (LinLin _ x y z) = x:y:z:Nil
subSignals (Mix _ x y z) = x:y:z:Nil
subSignals (LPF _ x y z) = x:y:z:Nil
subSignals (HPF _ x y z) = x:y:z:Nil
subSignals (BPF _ x y z) = x:y:z:Nil
subSignals (Delay _ x y)  = x:y:Nil
-- subSignals _ = Nil
-}

{-
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
dimensions Cama = { rows: 1, columns: 4 }
dimensions (Img _) = { rows: 1, columns: 3 }
dimensions (Imga _) = { rows: 1, columns: 4 }
dimensions (Vid _) = { rows: 1, columns: 3 }
dimensions (Vida _) = { rows: 1, columns: 4 }
dimensions (Gdm _) = { rows: 1, columns: 3 }
dimensions (Gdma _) = { rows: 1, columns: 4 }
dimensions (Rows _) = { rows: 1, columns: 1 }
dimensions (Cols _) = { rows: 1, columns: 1 }
dimensions (Chns _) = { rows: 1, columns: 1 }
dimensions (Flat x) = { rows: 1, columns: channels x }
dimensions (Trsp x) = { rows: d.columns, columns: d.rows }
  where d = dimensions x
dimensions (Get _ n x) = { rows: (dimensions x).rows, columns: max 1 n }
dimensions (Set mm n x y) = { rows: binaryFunctionChannels mm dx.rows dy.rows, columns: max (dx.columns + n) dy.columns } 
  where
    dx = dimensions x
    dy = dimensions y
dimensions (Map (SignalSignal f) x) = { rows: channels x, columns: channels (f.signalSignal $ Constant 0.0) }
dimensions (Rmap (SignalSignal f) x) = { rows: d.rows, columns: channels (f.signalSignal $ Rep d.columns (Constant 0.0)) }
  where d = dimensions x
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
dimensions (Spr mm x y) = binaryFunctionDimensions mm (channels y) (dimensions x).rows
dimensions (Btw mm n x y) = { columns: max 1 n, rows: binaryFunctionChannels mm (channels x) (channels y) }
dimensions (Pan mm n x y) = { columns: max 1 n, rows: binaryFunctionChannels mm (channels x) (channels y) }
dimensions (Splay n _) = { rows: 1, columns: max 1 n }
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

-- TODO: probably the order of arguments should be columns then rows here, not the other way around...
binaryFunctionDimensions :: MultiMode -> Int -> Int -> { columns :: Int, rows :: Int }
binaryFunctionDimensions Combinatorial rs 1 = {  columns: rs, rows: 1 } 
binaryFunctionDimensions Combinatorial rs cs = {  columns: cs, rows: rs }
binaryFunctionDimensions Pairwise x y = { columns: max x y, rows: 1 }

binaryFunctionChannels :: MultiMode -> Int -> Int -> Int
binaryFunctionChannels Combinatorial x y = x * y 
binaryFunctionChannels Pairwise x y = max x y

zero :: forall a. a -> Signal
zero _ = Constant 0.0
-}
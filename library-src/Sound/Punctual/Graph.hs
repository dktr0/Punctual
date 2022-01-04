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
  Px | Py | Aspect |
  Fx | Fy | Fxy |
  SetFx Graph Graph | SetFy Graph Graph | SetFxy Graph Graph |
  Zoom Graph Graph | Move Graph Graph | Tile Graph Graph | Spin Graph Graph |
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
  Ceil Graph |
  Fract Graph |
  Sum MultiMode Graph Graph |
  Product MultiMode Graph Graph |
  Division MultiMode Graph Graph |
  Pow MultiMode Graph Graph |
  Equal MultiMode Graph Graph |
  NotEqual MultiMode Graph Graph |
  GreaterThan MultiMode Graph Graph |
  GreaterThanOrEqual MultiMode Graph Graph |
  LessThan MultiMode Graph Graph |
  LessThanOrEqual MultiMode Graph Graph |
  Max Graph Graph |
  Min Graph Graph |
  Gate Graph Graph |
  Circle Graph Graph |
  Rect Graph Graph |
  Clip Graph Graph |
  Between Graph Graph |
  VLine Graph Graph |
  HLine Graph Graph |
  Step [Graph] Graph |
  IfThenElse Graph Graph Graph |
  ILine Graph Graph Graph |
  Line Graph Graph Graph |
  LinLin Graph Graph Graph |
  LPF Graph Graph Graph | HPF Graph Graph Graph | BPF Graph Graph Graph |
  Delay Double Graph Graph
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

modulatedRangeGraph :: Graph -> Graph -> Graph -> Graph
modulatedRangeGraph low high m = LinLin (Multi [-1,1]) (Multi [low,high]) m

(+-) :: Graph -> Graph -> Graph -> Graph
a +- b = modulatedRangeGraph (a - (a*b)) (a + (a*b))

fit :: Graph -> Graph -> Graph
fit ar x = IfThenElse ((GreaterThanOrEqual Combinatorial) Aspect ar) (Zoom (Multi [ar/Aspect,1]) $ x) (Zoom (Multi [1,Aspect/ar]) $ x)

multi :: [[a]] -> [[a]]
multi [] = []
multi (xs:[]) = fmap pure xs
multi (xs:ys) = [ x:y | x <- xs, y <- multi ys ]

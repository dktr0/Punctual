{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Graph where

import Data.Text (Text)
import Data.List
import GHC.Generics (Generic)
import Control.DeepSeq

data Graph =
  EmptyGraph |
  LocalBinding Int |
  Constant Double |
  Multi [Graph] |
  Mono Graph |
  Bipolar Graph |
  Unipolar Graph |
  Noise |
  Pink |
  Fx |
  Fy |
  Px |
  Py |
  TexR Graph Graph Graph |
  TexG Graph Graph Graph |
  TexB Graph Graph Graph |
  Lo | Mid | Hi |
  ILo | IMid | IHi |
  Sine Graph |
  Tri Graph |
  Saw Graph |
  Square Graph |
  LPF Graph Graph Graph |
  HPF Graph Graph Graph |
  FromTarget Text |
  Product Graph Graph |
  Sum Graph Graph |
  Mean Graph Graph |
  Max Graph Graph |
  Min Graph Graph |
  Division Graph Graph |
  GreaterThan Graph Graph |
  GreaterThanOrEqual Graph Graph |
  LessThan Graph Graph |
  LessThanOrEqual Graph Graph |
  Equal Graph Graph |
  NotEqual Graph Graph |
  Point Graph Graph |
  Distance Graph Graph |
  Circle Graph Graph Graph |
  Rect Graph Graph Graph Graph |
  MidiCps Graph |
  CpsMidi Graph |
  DbAmp Graph |
  AmpDb Graph |
  Abs Graph |
  Sqrt Graph |
  Pow Graph Graph |
  Floor Graph |
  Fract Graph |
  Clip Graph Graph Graph |
  Between Graph Graph Graph |
  VLine Graph Graph |
  HLine Graph Graph |
  ILine Graph Graph Graph Graph Graph |
  Line Graph Graph Graph Graph Graph |
  LinLin Graph Graph Graph Graph Graph
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

-- Multi-channel expansion:

expandMultis :: Graph -> [Graph]
expandMultis EmptyGraph = [EmptyGraph]
expandMultis (Multi []) = [EmptyGraph]
expandMultis (Multi xs) = fmap graphsToMono $ fmap expandMultis xs
expandMultis (Mono x) = [graphsToMono $ expandMultis x]
expandMultis (Bipolar x) = fmap Bipolar $ expandMultis x
expandMultis (Unipolar x) = fmap Unipolar $ expandMultis x
expandMultis (Sine x) = fmap Sine (expandMultis x)
expandMultis (Tri x) = fmap Tri (expandMultis x)
expandMultis (Saw x) = fmap Saw (expandMultis x)
expandMultis (Square x) = fmap Square (expandMultis x)
expandMultis (LPF i f q) = expandWith3 LPF i f q
expandMultis (HPF i f q) = expandWith3 HPF i f q
expandMultis (FromTarget x) = [Constant 0] -- placeholder
expandMultis (Product x y) = expandWith Product x y
expandMultis (TexR n x y) = expandWith3 TexR n x y
expandMultis (TexG n x y) = expandWith3 TexG n x y
expandMultis (TexB n x y) = expandWith3 TexB n x y
expandMultis (Sum x y) = expandWith Sum x y
expandMultis (Mean x y) = expandWith Mean x y
expandMultis (Max x y) = expandWith Max x y
expandMultis (Min x y) = expandWith Min x y
expandMultis (Division x y) = expandWith Division x y
expandMultis (GreaterThan x y) = expandWith GreaterThan x y
expandMultis (GreaterThanOrEqual x y) = expandWith GreaterThanOrEqual x y
expandMultis (LessThan x y) = expandWith LessThan x y
expandMultis (LessThanOrEqual x y) = expandWith LessThanOrEqual x y
expandMultis (Equal x y) = expandWith Equal x y
expandMultis (NotEqual x y) = expandWith NotEqual x y
expandMultis (Point x y) = expandWith Point x y
expandMultis (Distance x y) = expandWith Distance x y
expandMultis (Circle x y r) = expandWith3 Circle x y r
expandMultis (Rect x y w h) = expandWith4 Rect x y w h
expandMultis (MidiCps x) = fmap MidiCps (expandMultis x)
expandMultis (CpsMidi x) = fmap CpsMidi (expandMultis x)
expandMultis (DbAmp x) = fmap DbAmp (expandMultis x)
expandMultis (AmpDb x) = fmap AmpDb (expandMultis x)
expandMultis (Abs x) = fmap Abs (expandMultis x)
expandMultis (Sqrt x) = fmap Sqrt (expandMultis x)
expandMultis (Pow x y) = expandWith Pow x y
expandMultis (Floor x) = fmap Floor (expandMultis x)
expandMultis (Fract x) = fmap Fract (expandMultis x)
expandMultis (Clip x y z) = expandWith3 Clip x y z
expandMultis (Between r1 r2 x) = expandWith3 Between r1 r2 x
expandMultis (VLine x w) = expandWith VLine x w
expandMultis (HLine x w) = expandWith HLine x w
expandMultis (ILine x1 y1 x2 y2 w) = expandWith5 ILine x1 y1 x2 y2 w
expandMultis (Line x1 y1 x2 y2 w) = expandWith5 Line x1 y1 x2 y2 w
expandMultis (LinLin x1 y1 x2 y2 w) = expandWith5 LinLin x1 y1 x2 y2 w
expandMultis x = [x] -- everything else should, by definition, be a one-channel signal

graphsToMono :: [Graph] -> Graph
graphsToMono [] = EmptyGraph
graphsToMono xs = foldl1 Sum xs

-- Like zipWith... input graphs are multi-channel expanded, then cycled to the
-- length of whichever one of them is longest, then pairwise combined with the
-- given operator/function.

expandWith :: (Graph -> Graph -> Graph) -> Graph -> Graph -> [Graph]
expandWith f x y = zipWith f x'' y''
  where
    x' = expandMultis x
    y' = expandMultis y
    n = maximum [length x',length y']
    x'' = take n (cycle x')
    y'' = take n (cycle y')

expandWith3 :: (Graph -> Graph -> Graph -> Graph) -> Graph -> Graph -> Graph -> [Graph]
expandWith3 f x y z = zipWith3 f x'' y'' z''
  where
    x' = expandMultis x
    y' = expandMultis y
    z' = expandMultis z
    n = maximum [length x',length y',length z']
    x'' = take n (cycle x')
    y'' = take n (cycle y')
    z'' = take n (cycle z')

expandWith4 :: (Graph -> Graph -> Graph -> Graph -> Graph) -> Graph -> Graph -> Graph -> Graph -> [Graph]
expandWith4 f a b c d = zipWith4 f a'' b'' c'' d''
  where
    a' = expandMultis a
    b' = expandMultis b
    c' = expandMultis c
    d' = expandMultis d
    n = maximum [length a',length b',length c',length d']
    a'' = take n (cycle a')
    b'' = take n (cycle b')
    c'' = take n (cycle c')
    d'' = take n (cycle d')

expandWith5 :: (Graph -> Graph -> Graph -> Graph -> Graph -> Graph) -> Graph -> Graph -> Graph -> Graph -> Graph -> [Graph]
expandWith5 f a b c d e = zipWith5 f a'' b'' c'' d'' e''
  where
    a' = expandMultis a
    b' = expandMultis b
    c' = expandMultis c
    d' = expandMultis d
    e' = expandMultis e
    n = maximum [length a',length b',length c',length d',length e']
    a'' = take n (cycle a')
    b'' = take n (cycle b')
    c'' = take n (cycle c')
    d'' = take n (cycle d')
    e'' = take n (cycle e')

-- Miscellaneous functions over Graphs:

tex :: Graph -> Graph -> Graph -> Graph
tex n x y = Multi [TexR n x y,TexG n x y,TexB n x y]

fb :: Graph -> Graph -> Graph
fb x y = tex 0 x y

modulatedRangeGraph :: Graph -> Graph -> Graph -> Graph
modulatedRangeGraph low high m = LinLin (-1) (1) low high m

(+-) :: Graph -> Graph -> Graph -> Graph
a +- b = modulatedRangeGraph (a - (a*b)) (a + (a*b))

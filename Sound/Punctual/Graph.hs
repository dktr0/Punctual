module Sound.Punctual.Graph where

import Data.Text (Text)

data Graph =
  EmptyGraph |
  Constant Double |
  Multi [Graph] |
  Mono Graph |
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
  Sine Graph |
  Tri Graph |
  Saw Graph |
  Square Graph |
  LPF Graph Graph Graph |
  HPF Graph Graph Graph |
  FromTarget Text |
  Product Graph Graph |
  Sum Graph Graph |
  Division Graph Graph |
  GreaterThan Graph Graph |
  GreaterThanOrEqual Graph Graph |
  LessThan Graph Graph |
  LessThanOrEqual Graph Graph |
  Equal Graph Graph |
  NotEqual Graph Graph |
  MidiCps Graph |
  CpsMidi Graph |
  DbAmp Graph |
  AmpDb Graph |
  Abs Graph |
  Sqrt Graph |
  Pow Graph Graph |
  Floor Graph |
  Fract Graph |
  Clip Graph Graph Graph
  deriving (Show,Eq)

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
expandMultis (Multi []) = [EmptyGraph]
-- expandMultis (Multi xs) = fmap mixIfMulti xs
expandMultis (Multi xs) = fmap graphsToMono $ fmap expandMultis xs
expandMultis (Mono x) = [graphsToMono $ expandMultis x]
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
expandMultis (Division x y) = expandWith Division x y
expandMultis (GreaterThan x y) = expandWith GreaterThan x y
expandMultis (GreaterThanOrEqual x y) = expandWith GreaterThanOrEqual x y
expandMultis (LessThan x y) = expandWith LessThan x y
expandMultis (LessThanOrEqual x y) = expandWith LessThanOrEqual x y
expandMultis (Equal x y) = expandWith Equal x y
expandMultis (NotEqual x y) = expandWith NotEqual x y
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
expandMultis x = [x] -- everything else should, by definition, be a one-channel signal

-- mixIfMulti :: Graph -> Graph
-- mixIfMulti (Multi xs) = graphsToMono xs
-- mixIfMulti x = x

graphsToMono :: [Graph] -> Graph
graphsToMono [] = EmptyGraph
graphsToMono (x:[]) = x
graphsToMono xs = foldl Sum EmptyGraph xs

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


-- Miscellaneous functions over Graphs:

tex :: Graph -> Graph -> Graph -> Graph
tex n x y = Multi [TexR n x y,TexG n x y,TexB n x y]

fb :: Graph
fb = tex 0 Fx Fy

bipolar :: Graph -> Graph
bipolar x = x * 2 - 1

unipolar :: Graph -> Graph
unipolar x = x * 0.5 + 0.5

mean :: Graph -> Graph -> Graph
mean x y = (x + y) * 0.5

linlin :: Graph -> Graph -> Graph -> Graph -> Graph -> Graph
linlin min1 max1 min2 max2 x = min2 + (outputRange * proportion)
  where
    inputRange = max1 - min1
    outputRange = max2 - min2
    proportion = Division (x - min1) inputRange

modulatedRangeGraph :: Graph -> Graph -> Graph -> Graph
modulatedRangeGraph low high m = (mean low high) + ((high - low) * 0.5 * m)

rect :: Graph -> Graph -> Graph -> Graph -> Graph
rect x y w h = Product inHrange inVrange
  where
    x0 = Sum x (Product w (Constant (-0.5)))
    x1 = Sum x (Product w (Constant (0.5)))
    y0 = Sum y (Product h (Constant (-0.5)))
    y1 = Sum y (Product h (Constant (0.5)))
    inHrange = Product (GreaterThanOrEqual Fx x0) (LessThanOrEqual Fx x1)
    inVrange = Product (GreaterThanOrEqual Fy y0) (LessThanOrEqual Fy y1)

point :: Graph -> Graph -> Graph
point x y = Product inHrange inVrange
  where
    x0 = Sum x (Product Px (Constant (-0.5)))
    x1 = Sum x (Product Px (Constant (0.5)))
    y0 = Sum y (Product Py (Constant (-0.5)))
    y1 = Sum y (Product Py (Constant (0.5)))
    inHrange = Product (GreaterThanOrEqual Fx x0) (LessThanOrEqual Fx x1)
    inVrange = Product (GreaterThanOrEqual Fy y0) (LessThanOrEqual Fy y1)

hline :: Graph -> Graph
hline y = Product (GreaterThanOrEqual Fy y0) (LessThanOrEqual Fy y1)
  where
    y0 = Sum y (Product Py (Constant (-0.5)))
    y1 = Sum y (Product Py (Constant (0.5)))

vline :: Graph -> Graph
vline x = Product (GreaterThanOrEqual Fx x0) (LessThanOrEqual Fx x1)
  where
    x0 = Sum x (Product Px (Constant (-0.5)))
    x1 = Sum x (Product Px (Constant (0.5)))

squared :: Graph -> Graph
squared x = Product x x

distance :: Graph -> Graph -> Graph
distance x y = Sqrt (squared (Fx-x) + squared (Fy-y))

circle :: Graph -> Graph -> Graph -> Graph
circle x y r = LessThanOrEqual (distance x y) r

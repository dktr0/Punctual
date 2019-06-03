module Sound.Punctual.Graph where

data Graph =
  EmptyGraph |
  Constant Double |
  Multi [Graph] |
  Noise |
  Pink |
  Fx |
  Fy |
  Px |
  Py |
  Sine Graph |
  Tri Graph |
  Saw Graph |
  Square Graph |
  LPF Graph Graph Graph |
  HPF Graph Graph Graph |
  FromTarget String |
  Product Graph Graph |
  Sum Graph Graph |
  Division Graph Graph |
  GreaterThan Graph Graph |
  GreaterThanOrEqual Graph Graph |
  LessThan Graph Graph |
  LessThanOrEqual Graph Graph |
  Equal Graph Graph |
  NotEqual Graph Graph
  deriving (Show,Eq)

expandMultis :: Graph -> [Graph]
expandMultis (Multi []) = [EmptyGraph]
expandMultis (Multi xs) = fmap mixIfMulti xs
expandMultis (Sine x) = fmap Sine (expandMultis x)
expandMultis (Tri x) = fmap Tri (expandMultis x)
expandMultis (Saw x) = fmap Saw (expandMultis x)
expandMultis (Square x) = fmap Square (expandMultis x)
expandMultis (LPF i f q) = expandWith3 LPF i f q
expandMultis (HPF i f q) = expandWith3 HPF i f q
expandMultis (FromTarget x) = [Constant 0] -- placeholder
expandMultis (Product x y) = expandWith Product x y
expandMultis (Sum x y) = expandWith Sum x y
expandMultis (Division x y) = expandWith Division x y
expandMultis (GreaterThan x y) = expandWith GreaterThan x y
expandMultis (GreaterThanOrEqual x y) = expandWith GreaterThanOrEqual x y
expandMultis (LessThan x y) = expandWith LessThan x y
expandMultis (LessThanOrEqual x y) = expandWith LessThanOrEqual x y
expandMultis (Equal x y) = expandWith Equal x y
expandMultis (NotEqual x y) = expandWith NotEqual x y
expandMultis x = [x] -- everything else should, by definition, be a one-channel signal

mixIfMulti :: Graph -> Graph
mixIfMulti (Multi xs) = mixGraphs xs
mixIfMulti x = x

mixGraphs :: [Graph] -> Graph
mixGraphs xs = foldl Sum EmptyGraph xs

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

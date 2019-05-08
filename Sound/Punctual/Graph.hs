module Sound.Punctual.Graph where

data Graph =
  EmptyGraph |
  Constant Double |
  Noise |
  Pink |
  Fx |
  Fy |
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

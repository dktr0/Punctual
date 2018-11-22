module Sound.Punctual.Graph where

data Graph =
  EmptyGraph |
  Constant Double |
  Noise |
  Pink |
  Sine Graph |
  Tri Graph |
  Saw Graph |
  Square Graph |
  Pulse Graph |
  LPF Graph Graph Graph |
  HPF Graph Graph Graph |
  FromTarget String |
  Product Graph Graph |
  Sum Graph Graph
  deriving (Show,Eq)

module Sound.Punctual.Graph where

import Sound.Punctual.Token
import Sound.Punctual.Extent

data Graph =
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
  Mix [Graph] |
  EmptyGraph |
  FromTarget String |
  Product Graph Graph |
  Sum Graph Graph
  deriving (Show,Eq)

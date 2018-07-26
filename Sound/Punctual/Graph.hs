module Sound.Punctual.Graph where

import Text.ParserCombinators.Parsec
import Sound.Punctual.Token

-- import Text.ParserCombinators.Parsec.Number
-- import Text.Parsec.Number

data Graph =
  Constant Double |
  Noise | Pink |
  Sine Graph | Tri Graph | Saw Graph | Square Graph | Pulse Graph |
  LPF Graph Graph Graph | HPF Graph Graph Graph |
  ADSR Graph Graph Graph Graph | -- attack decay gate release
  Mix [Graph] |
  EmptyGraph |
  FromTarget String |
  Product Graph Graph |
  Sum Graph Graph
  deriving (Show,Eq)

graph :: GenParser Char a Graph
graph = sumOfGraphs <|> return EmptyGraph

sumOfGraphs :: GenParser Char a Graph
sumOfGraphs = chainl1 productOfGraphs sumOp

sumOp :: GenParser Char a (Graph -> Graph -> Graph)
sumOp = reservedOp "+" >> return Sum

productOfGraphs :: GenParser Char a Graph
productOfGraphs = chainl1 simpleGraph productOp

productOp :: GenParser Char a (Graph -> Graph -> Graph)
productOp = reservedOp "*" >> return Product

simpleGraph :: GenParser Char a Graph
simpleGraph = choice [
    parens sumOfGraphs,
    Constant <$> double,
    reserved "noise" >> return Noise,
    reserved "pink" >> return Pink,
    oscillators,
    filters,
    envelopes,
    mixGraph,
    FromTarget <$> identifier
    ]

oscillators :: GenParser Char a Graph
oscillators = choice [
  Sine <$> (reserved "sine" >> spaces >> simpleGraph),
  Tri <$> (reserved "tri" >> spaces >> simpleGraph),
  Saw <$> (reserved "saw" >> spaces >> simpleGraph),
  Square <$> (reserved "square" >> spaces >> simpleGraph),
  Pulse <$> (reserved "pulse" >> spaces >> simpleGraph)
  ]

filters :: GenParser Char a Graph
filters = do
  x <- (reserved "lpf" >> return LPF) <|> (reserved "hpf" >> return HPF)
  x <$> simpleGraph <*> simpleGraph <*> simpleGraph

envelopes :: GenParser Char a Graph
envelopes = choice [
  reserved "adsr" >> (ADSR <$> simpleGraph <*> simpleGraph <*> simpleGraph <*> simpleGraph)
  ]

mixGraph :: GenParser Char a Graph
mixGraph = reserved "mix" >> (Mix <$> (brackets (commaSep sumOfGraphs)))

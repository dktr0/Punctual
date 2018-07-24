module Sound.Punctual.Graph where

import Text.ParserCombinators.Parsec
-- import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Number

-- *NEXT* multichannel expansion notations

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
sumOp = spaces >> char '+' >> return Sum

productOfGraphs :: GenParser Char a Graph
productOfGraphs = chainl1 simpleGraph productOp

productOp :: GenParser Char a (Graph -> Graph -> Graph)
productOp = spaces >> char '*' >> return Product

simpleGraph :: GenParser Char a Graph
simpleGraph = do
  spaces
  x <- choice [
    inBrackets sumOfGraphs,
    Constant <$> double,
    try $ string "noise" >> return Noise,
    try $ string "pink" >> return Pink,
    try oscillators,
    try filters,
    try envelopes,
    try mixGraph,
    FromTarget <$> many1 letter
    ]
  spaces
  return x

oscillators :: GenParser Char a Graph
oscillators = choice [
  try $ Sine <$> (string "sine" >> spaces >> simpleGraph),
  try $ Tri <$> (string "tri" >> spaces >> simpleGraph),
  try $ Saw <$> (string "saw" >> spaces >> simpleGraph),
  try $ Square <$> (string "square" >> spaces >> simpleGraph),
  try $ Pulse <$> (string "pulse" >> spaces >> simpleGraph)
  ]

filters :: GenParser Char a Graph
filters = do
  x <- (string "lpf" >> return LPF) <|> (string "hpf" >> return HPF)
  x <$> simpleGraph <*> simpleGraph <*> simpleGraph

envelopes :: GenParser Char a Graph
envelopes = choice [
  string "adsr" >> (ADSR <$> simpleGraph <*> simpleGraph <*> simpleGraph <*> simpleGraph)
  ]

mixGraph :: GenParser Char a Graph
mixGraph = string "mix" >> spaces >> (Mix <$> listOfGraphs)

listOfGraphs :: GenParser Char a [Graph]
listOfGraphs = do
  char '['
  x <- sepBy sumOfGraphs (char ',')
  char ']'
  return x

inBrackets :: GenParser Char a b -> GenParser Char a b
inBrackets p = do
  char '('
  spaces
  x <- p
  spaces
  char ')'
  return x

double :: GenParser Char a Double
double = choice [
  try (char '-' >> fractional3 False >>= return . (* (-1))),
  fractional3 False
  ]

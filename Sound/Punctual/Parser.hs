module Sound.Punctual.Parser (runPunctualParser) where

import Text.ParserCombinators.Parsec
import Sound.Punctual.Token
import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Types

duration :: GenParser Char a Duration
duration = choice $ fmap try [seconds,milliseconds,cycles]

seconds :: GenParser Char a Duration
seconds = do
  x <- double
  reserved "s"
  return $ Seconds x

milliseconds :: GenParser Char a Duration
milliseconds = do
  x <- double
  reserved "ms"
  return $ Seconds (x/1000.0)

cycles :: GenParser Char a Duration
cycles = do
  x <- double
  reserved "c"
  return $ Cycles x

defTimeParser :: GenParser Char a DefTime
defTimeParser = do
  reservedOp "@"
  (After <$> duration) <|> quant

quant :: GenParser Char a DefTime
quant = parens $ do
  x <- double
  comma
  y <- duration
  return $ Quant x y

transitionParser :: GenParser Char a Transition
transitionParser = choice [
  reservedOp "<>" >> return DefaultCrossFade,
  reservedOp "~" >> return HoldPhase,
  reservedOp "=" >> return (CrossFade (Seconds 0.0)),
  CrossFade <$> angles duration
  ]

definitionParser :: GenParser Char a Definition
definitionParser = do
  a <- option Anonymous $ (Explicit <$> identifier)
  b <- option (After (Seconds 0)) defTimeParser
  c <- option DefaultCrossFade  transitionParser
  d <- graphParser
  return $ Definition a b c d

outputParser :: GenParser Char a Output
outputParser = choice [
  try $ reservedOp ":" >> (PannedOutput <$> extent),
  reservedOp ":" >> return (PannedOutput 0.5),
  return NoOutput
  ]

expression :: GenParser Char a Expression
expression = Expression <$> definitionParser <*> outputParser

punctualParser :: GenParser Char a [Expression]
punctualParser = do
  whiteSpace
  x <- expression `sepBy` reservedOp ";"
  eof
  return x

runPunctualParser :: String -> Either ParseError [Expression]
runPunctualParser = parse punctualParser ""

graphParser :: GenParser Char a Graph
graphParser = sumOfGraphs <|> return EmptyGraph

sumOfGraphs :: GenParser Char a Graph
sumOfGraphs = chainl1 productOfGraphs $ choice [
  reservedOp "+" >> return Sum,
  reservedOp "-" >> return (\x y -> Sum x (Product y (Constant (-1))))
  ]

productOfGraphs :: GenParser Char a Graph
productOfGraphs = chainl1 simpleGraph (reservedOp "*" >> return Product)

simpleGraph :: GenParser Char a Graph
simpleGraph = choice [
    parens graphParser,
    Constant <$> extent,
    reserved "noise" >> return Noise,
    reserved "pink" >> return Pink,
    oscillators,
    filters,
    mixGraph,
    FromTarget <$> lexeme identifier
    ]

oscillators :: GenParser Char a Graph
oscillators = choice [
  Sine <$> (reserved "sine" >> simpleGraph),
  Tri <$> (reserved "tri" >> simpleGraph),
  Saw <$> (reserved "saw" >> simpleGraph),
  Square <$> (reserved "square" >> simpleGraph)
  ]

filters :: GenParser Char a Graph
filters = do
  x <- (reserved "lpf" >> return LPF) <|> (reserved "hpf" >> return HPF)
  x <$> simpleGraph <*> simpleGraph <*> simpleGraph

mixGraph :: GenParser Char a Graph
mixGraph = do
  reserved "mix"
  xs <- brackets (commaSep sumOfGraphs)
  return $ foldl Sum EmptyGraph xs

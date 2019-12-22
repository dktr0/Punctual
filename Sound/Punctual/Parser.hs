{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Parser (runPunctualParser) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import Sound.Punctual.Token
import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Types

duration :: Parser Duration
duration = choice $ fmap try [seconds,milliseconds,cycles]

seconds :: Parser Duration
seconds = do
  x <- double
  reserved "s"
  return $ Seconds x

milliseconds :: Parser Duration
milliseconds = do
  x <- double
  reserved "ms"
  return $ Seconds (x/1000.0)

cycles :: Parser Duration
cycles = do
  x <- double
  reserved "c"
  return $ Cycles x

cyclesInQuant :: Parser Double
cyclesInQuant = do
  x <- double
  reserved "c"
  return x

defTimeParser :: Parser DefTime
defTimeParser = reservedOp "@" >> choice [
  try $ quant,
  try $ cyclesInQuant >>= \n -> return (Quant n (Seconds 0.0)),
  try $ seconds >>= return . After,
  try $ milliseconds >>= return . After
  ]

quant :: Parser DefTime
quant = do
  x <- cyclesInQuant
  reservedOp "+"
  y <- duration
  return $ Quant x y

transitionParser :: Parser Transition
transitionParser = choice [
  reservedOp "<>" >> return DefaultCrossFade,
  reservedOp "~" >> return HoldPhase,
  reservedOp "=" >> return (CrossFade (Seconds 0.0)),
  CrossFade <$> angles duration
  ]

definitionParser :: Parser Definition
definitionParser = do
  a <- option Anonymous $ (Explicit . T.pack <$> identifier)
  b <- option (Quant 1 (Seconds 0)) defTimeParser
  c <- option DefaultCrossFade  transitionParser
  d <- graphParser
  return $ Definition a b c d

outputParser :: Parser Output
outputParser = choice [
  try $ reservedOp "=>" >> (PannedOutput <$> extent),
  try $ reservedOp "=>" >> reserved "left" >> return (PannedOutput 0),
  try $ reservedOp "=>" >> reserved "right" >> return (PannedOutput 1),
  try $ reservedOp "=>" >> reserved "centre" >> return (PannedOutput 0.5),
  try $ reservedOp "=>" >> reserved "splay" >> return (NamedOutput "splay"),
  try $ reservedOp "=>" >> reserved "red" >> return (NamedOutput "red"),
  try $ reservedOp "=>" >> reserved "green" >> return (NamedOutput "green"),
  try $ reservedOp "=>" >> reserved "blue" >> return (NamedOutput "blue"),
  try $ reservedOp "=>" >> reserved "alpha" >> return (NamedOutput "alpha"),
  try $ reservedOp "=>" >> reserved "rgb" >> return (NamedOutput "rgb"),
  try $ reservedOp "=>" >> reserved "hsv" >> return (NamedOutput "hsv"),
  return NoOutput
  ]

expression :: Parser Expression
expression = Expression <$> definitionParser <*> outputParser

punctualParser :: Parser [Expression]
punctualParser = do
  whiteSpace
  x <- expression `sepBy` reservedOp ";"
  eof
  return x

runPunctualParser :: Text -> Either ParseError [Expression]
runPunctualParser = parse punctualParser ""

graphParser :: Parser Graph
graphParser = sumOfGraphs <|> return EmptyGraph

sumOfGraphs :: Parser Graph
sumOfGraphs = chainl1 comparisonOfGraphs $ choice [
  reservedOp "+" >> return (+),
  reservedOp "-" >> return (-)
  ]

comparisonOfGraphs :: Parser Graph
comparisonOfGraphs = chainl1 productOfGraphs $ choice [
  reservedOp ">" >> return GreaterThan,
  reservedOp "<" >> return LessThan,
  reservedOp ">=" >> return GreaterThanOrEqual,
  reservedOp "<=" >> return LessThanOrEqual,
  reservedOp "==" >> return Equal,
  reservedOp "!=" >> return NotEqual
  ]

productOfGraphs :: Parser Graph
productOfGraphs = chainl1 simpleGraph $ choice [
  reservedOp "**" >> return Pow,
  reservedOp "*" >> return Product,
  reservedOp "/" >> return Division
  ]

simpleGraph :: Parser Graph
simpleGraph = choice [
  try $ modulatedRange,
  try $ multiSeries,
  functionsWithArguments,
  graphArgument
  ]

graphArgument :: Parser Graph
graphArgument = choice [
  try $ parens graphParser,
  multiGraph,
  Constant <$> try extent,
  functionsWithoutArguments
  ]

functionsWithoutArguments :: Parser Graph
functionsWithoutArguments = choice [
  reserved "noise" >> return Noise,
  reserved "pink" >> return Pink,
  reserved "fx" >> return Fx,
  reserved "fy" >> return Fy,
  reserved "px" >> return Px,
  reserved "py" >> return Py,
  reserved "lo" >> return Lo,
  reserved "mid" >> return Mid,
  reserved "hi" >> return Hi,
  reserved "fb" >> return fb
  ]

functionsWithArguments :: Parser Graph
functionsWithArguments = choice [
  (reserved "bipolar" >> return bipolar) <*> graphArgument,
  (reserved "unipolar" >> return unipolar) <*> graphArgument,
  Sine <$> (reserved "sin" >> graphArgument),
  Tri <$> (reserved "tri" >> graphArgument),
  Saw <$> (reserved "saw" >> graphArgument),
  Square <$> (reserved "sqr" >> graphArgument),
  (reserved "lpf" >> return LPF) <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "hpf" >> return HPF) <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "mono" >> return Mono) <*> graphArgument,
  (reserved "abs" >> return Abs) <*> graphArgument,
  (reserved "cpsmidi" >> return CpsMidi) <*> graphArgument,
  (reserved "midicps" >> return MidiCps) <*> graphArgument,
  (reserved "dbamp" >> return DbAmp) <*> graphArgument,
  (reserved "ampdb" >> return AmpDb) <*> graphArgument,
  (reserved "mean" >> return mean) <*> graphArgument <*> graphArgument,
  (reserved "squared" >> return squared) <*> graphArgument,
  (reserved "sqrt" >> return Sqrt) <*> graphArgument,
  (reserved "distance" >> return distance) <*> graphArgument <*> graphArgument,
  (reserved "circle" >> return circle) <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "point" >> return point) <*> graphArgument <*> graphArgument,
  (reserved "hline" >> return hline) <*> graphArgument,
  (reserved "vline" >> return vline) <*> graphArgument,
  (reserved "linlin" >> return linlin) <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "rect" >> return rect) <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "texr" >> return TexR) <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "texg" >> return TexG) <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "texb" >> return TexB) <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "tex" >> return tex) <*> graphArgument <*> graphArgument <*> graphArgument
  ]

modulatedRange :: Parser Graph
modulatedRange = do
  (a,b) <- rangeParser
  reservedOp ":"
  m <- graphParser
  return $ modulatedRangeGraph a b m

rangeParser :: Parser (Graph,Graph)
rangeParser = choice [
  try $ do
    x <- graphArgument
    reservedOp "+-"
    y <- graphArgument
    let y' = Product x y
    return (x - y', x + y'),
  try $ do
    x <- graphArgument
    reservedOp "->"
    y <- graphArgument
    return (x,y),
  try $ do
    x <- graphArgument
    return (Constant 0,x)
  ]

multiGraph :: Parser Graph
multiGraph = choice [
  try $ multiGraph' >>= (\x -> reserved "db" >> return (DbAmp x)),
  try $ multiGraph' >>= (\x -> reserved "m" >> return (MidiCps x)),
  multiGraph'
  ]

multiGraph' :: Parser Graph
multiGraph' = brackets (commaSep graphParser) >>= return . Multi

multiSeries :: Parser Graph
multiSeries = do
  x <- fromIntegral <$> integer
  reservedOp ".."
  y <- fromIntegral <$> integer
  return $ Multi $ fmap Constant [x .. y]

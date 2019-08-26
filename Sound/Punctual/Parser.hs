{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Parser (runParser) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (runParser)
import Text.Parsec.Text
import Data.IntMap.Strict

import Sound.Punctual.Token
import Sound.Punctual.Extent
import Sound.Punctual.Duration
import Sound.Punctual.DefTime
import Sound.Punctual.Transition
import Sound.Punctual.Graph
import Sound.Punctual.Target
import Sound.Punctual.Definition
import Sound.Punctual.Program


extent :: Parser Extent
extent = choice $ fmap try [extentDb,extentPercent,extentMidi,double]

extentDb :: Parser Extent
extentDb = do
  x <- double
  reserved "db"
  return $ dbamp x

extentPercent :: Parser Extent
extentPercent = do
  x <- double
  reservedOp "%"
  return $ x / 100

extentMidi :: Parser Extent
extentMidi = do
  x <- double
  reserved "m"
  return $ midicps x

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
  a <- option (Quant 1 (Seconds 0)) defTimeParser
  b <- option DefaultCrossFade  transitionParser
  c <- graphParser
  d <- targetsParser
  return $ Definition a b c d

targetsParser :: Parser [Target]
targetsParser = choice [
  reservedOp "=>" >> targetParser `sepBy` reservedOp "," ,
  return []
  ]

targetParser :: Parser Target
targetParser = choice [
  Panned <$> extent,
  reserved "left" >> return (Panned 0),
  reserved "right" >> return (Panned 1),
  reserved "centre" >> return (Panned 0.5),
  reserved "splay" >> return Splay,
  reserved "red" >> return Red,
  reserved "green" >> return Green,
  reserved "blue" >> return Blue,
  reserved "alpha" >> return Alpha,
  reserved "rgb" >> return RGB,
  identifier >>= return . NamedTarget . T.pack
  ]

programParser :: Parser Program
programParser = do
  whiteSpace
  x <- definitionParser `sepBy` reservedOp ";"
  eof
  return $ fromList $ zip [0..] x

runParser :: Text -> Either ParseError Program
runParser = parse programParser ""

graphParser :: Parser Graph
graphParser = sumOfGraphs <|> return EmptyGraph

sumOfGraphs :: Parser Graph
sumOfGraphs = chainl1 comparisonOfGraphs $ choice [
  reservedOp "+" >> return Sum,
  reservedOp "-" >> return (\x y -> Sum x (Product y (Constant (-1))))
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
  reservedOp "*" >> return Product,
  reservedOp "/" >> return Division
  ]

simpleGraph :: Parser Graph
simpleGraph = choice [
  graphArgument,
  try $ modulatedRange,
  functionsWithArguments
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
  reserved "fx" >> return Fx,
  reserved "fy" >> return Fy,
  reserved "px" >> return Px,
  reserved "py" >> return Py
  ]

functionsWithArguments :: Parser Graph
functionsWithArguments = choice [
  -- Graph constructors
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
  (reserved "sqrt" >> return Sqrt) <*> graphArgument,
  -- "synthetic" functions (ie. those that are not graph constructors)
  (reserved "bipolar" >> return bipolar) <*> graphArgument,
  (reserved "unipolar" >> return unipolar) <*> graphArgument,
  (reserved "mean" >> return mean) <*> graphArgument <*> graphArgument,
  (reserved "squared" >> return squared) <*> graphArgument,
  (reserved "distance" >> return distance) <*> graphArgument <*> graphArgument,
  (reserved "circle" >> return circle) <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "point" >> return point) <*> graphArgument <*> graphArgument,
  (reserved "hline" >> return hline) <*> graphArgument,
  (reserved "vline" >> return vline) <*> graphArgument,
  (reserved "linlin" >> return linlin) <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument,
  (reserved "rect" >> return rect) <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument
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
    reservedOp ".."
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

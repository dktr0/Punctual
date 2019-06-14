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

cyclesInQuant :: GenParser Char a Double
cyclesInQuant = do
  x <- double
  reserved "c"
  return x

-- Definitions (and transitions):
-- a <> sin 660 -- default crossfade, at closest possible 1-cycle boundary
-- a <2s> sin 660 -- a 2 second crossfade, at closest possible 1-cycle boundary
-- a <3c> sin 880 -- a 3-cycle crossfade, at closest possible 1-cycle boundary
-- a @4c sin 880 -- replace the definition at the next 4-cycle boundary
-- a @4c+0.5c sin 880 -- replace 0.5 cycles after next 4-cycle boundary
-- a @4c+0.5s sin 880 -- replace 0.5 seconds after next 4-cycle boundary
-- a @2s sin 880 -- replace the definition 2 seconds from "now"
-- a @2s <4s> sin 990 -- a 4 second crossfade, starting 2 seconds from now
-- a @0s <4s> sin 990 -- a 4 second crossfade, starting immediately
-- a @0c <4s> sin 990 -- also a 4 second crossfade, starting immediately
-- a <10s>         -- a 10-second fade out, centred on closest possible 1-cycle boundary
-- <2s> sin 440 -- target is anonymous
-- sin 440 -- target is anonymous and transition is default crossfade

defTimeParser :: GenParser Char a DefTime
defTimeParser = reservedOp "@" >> choice [
  try $ quant,
  try $ cyclesInQuant >>= \n -> return (Quant n (Seconds 0.0)),
  try $ seconds >>= return . After,
  try $ milliseconds >>= return . After
  ]

quant :: GenParser Char a DefTime
quant = do
  x <- cyclesInQuant
  reservedOp "+"
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
  b <- option (Quant 1 (Seconds 0)) defTimeParser
  c <- option DefaultCrossFade  transitionParser
  d <- graphParser
  return $ Definition a b c d

outputParser :: GenParser Char a Output
outputParser = choice [
  try $ reservedOp "=>" >> (PannedOutput <$> extent),
  try $ reservedOp "=>" >> reserved "left" >> return (PannedOutput 0),
  try $ reservedOp "=>" >> reserved "right" >> return (PannedOutput 1),
  try $ reservedOp "=>" >> reserved "centre" >> return (PannedOutput 0.5),
  try $ reservedOp "=>" >> reserved "x" >> return (NamedOutput "x"),
  try $ reservedOp "=>" >> reserved "y" >> return (NamedOutput "y"),
  try $ reservedOp "=>" >> reserved "red" >> return (NamedOutput "red"),
  try $ reservedOp "=>" >> reserved "green" >> return (NamedOutput "green"),
  try $ reservedOp "=>" >> reserved "blue" >> return (NamedOutput "blue"),
  try $ reservedOp "=>" >> reserved "alpha" >> return (NamedOutput "alpha"),
  try $ reservedOp "=>" >> reserved "rgb" >> return (NamedOutput "rgb"),
  try $ reservedOp "=>" >> reserved "clear" >> return (NamedOutput "clear"),
  try $ reservedOp "=>" >> reserved "width" >> return (NamedOutput "width"),
  try $ reservedOp "=>" >> reserved "height" >> return (NamedOutput "height"),
  try $ reservedOp "=>" >> return (PannedOutput 0.5),
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
sumOfGraphs = chainl1 comparisonOfGraphs $ choice [
  reservedOp "+" >> return Sum,
  reservedOp "-" >> return (\x y -> Sum x (Product y (Constant (-1))))
  ]

comparisonOfGraphs :: GenParser Char a Graph
comparisonOfGraphs = chainl1 productOfGraphs $ choice [
  reservedOp ">" >> return GreaterThan,
  reservedOp "<" >> return LessThan,
  reservedOp ">=" >> return GreaterThanOrEqual,
  reservedOp "<=" >> return LessThanOrEqual,
  reservedOp "==" >> return Equal,
  reservedOp "!=" >> return NotEqual
  ]

productOfGraphs :: GenParser Char a Graph
productOfGraphs = chainl1 simpleGraph $ choice [
  reservedOp "*" >> return Product,
  reservedOp "/" >> return Division
  ]

simpleGraph :: GenParser Char a Graph
simpleGraph = choice [
    try $ modulatedRange,
    Constant <$> extent,
    reserved "noise" >> return Noise,
    reserved "pink" >> return Pink,
    reserved "fx" >> return Fx,
    reserved "fy" >> return Fy,
    reserved "px" >> return Px,
    reserved "py" >> return Py,
    (reserved "abs" >> return Abs) <*> graphArgument,
    (reserved "cpsmidi" >> return CpsMidi) <*> graphArgument,
    (reserved "midicps" >> return MidiCps) <*> graphArgument,
    (reserved "dbamp" >> return DbAmp) <*> graphArgument,
    (reserved "ampdb" >> return AmpDb) <*> graphArgument,
    (reserved "point" >> return point) <*> graphArgument <*> graphArgument,
    (reserved "hline" >> return hline) <*> graphArgument,
    (reserved "vline" >> return vline) <*> graphArgument,
    (reserved "linlin" >> return linlin) <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument,
    (reserved "rect" >> return rect) <*> graphArgument <*> graphArgument <*> graphArgument <*> graphArgument,
    oscillators,
    filters,
    mixGraph,
    multiGraph,
    functions,
    -- FromTarget <$> lexeme identifier
    try $ parens graphParser
    ]

linlin :: Graph -> Graph -> Graph -> Graph -> Graph -> Graph
linlin min1 max1 min2 max2 x = Sum min2 (Product outputRange proportion)
  where
    inputRange = difference max1 min1
    outputRange = difference max2 min2
    proportion = Division (difference x min1) inputRange

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

graphArgument :: GenParser Char a Graph
graphArgument = choice [
  try $ parens graphParser,
  multiGraph,
  Constant <$> try extent,
  reserved "noise" >> return Noise,
  reserved "pink" >> return Pink,
  reserved "fx" >> return Fx,
  reserved "fy" >> return Fy
  ]

--  x <> 440 +- 2% <- sin 0.5
--  x <> 440 +- 10 <- sin 0.5
--  x <> 430 .. 450 <- sin 0.5

modulatedRange :: GenParser Char a Graph
modulatedRange = do
  (a,b) <- rangeParser
  reservedOp ":"
  m <- graphParser
  return $ modulatedRangeGraph a b m

modulatedRangeGraph :: Graph -> Graph -> Graph -> Graph
modulatedRangeGraph low high mod = Sum (average low high) (Product (Product (difference high low) (Constant 0.5)) mod)

average :: Graph -> Graph -> Graph
average x y = Product (Sum x y) (Constant 0.5)

rangeParser :: GenParser Char a (Graph,Graph)
rangeParser = choice [
  try $ do
    x <- graphArgument
    reservedOp "+-"
    y <- graphArgument
    let y' = Product x y
    return (difference x y',Sum x y'),
  try $ do
    x <- graphArgument
    reservedOp ".."
    y <- graphArgument
    return (x,y),
  try $ do
    x <- graphArgument
    return (Constant 0,x)
  ]

difference :: Graph -> Graph -> Graph
difference x y = Sum x (Product y (Constant (-1)))

oscillators :: GenParser Char a Graph
oscillators = choice [
  Sine <$> (reserved "sin" >> graphArgument),
  Tri <$> (reserved "tri" >> graphArgument),
  Saw <$> (reserved "saw" >> graphArgument),
  Square <$> (reserved "sqr" >> graphArgument)
  ]

filters :: GenParser Char a Graph
filters = do
  x <- (reserved "lpf" >> return LPF) <|> (reserved "hpf" >> return HPF)
  x <$> graphArgument <*> graphArgument <*> graphArgument

mixGraph :: GenParser Char a Graph
mixGraph = do
  reserved "mix"
  mixGraphs <$> brackets (commaSep graphParser)

multiGraph :: GenParser Char a Graph
multiGraph = choice [
  try $ multiGraph' >>= (\x -> reserved "db" >> return (DbAmp x)),
  try $ multiGraph' >>= (\x -> reserved "m" >> return (MidiCps x)),
  multiGraph'
  ]

multiGraph' :: GenParser Char a Graph
multiGraph' = brackets (commaSep graphParser) >>= return . Multi

functions :: GenParser Char a Graph
functions = choice [
  (reserved "bipolar" >> return bipolar) <*> graphArgument,
  (reserved "unipolar" >> return unipolar) <*> graphArgument
  ]

bipolar :: Graph -> Graph
bipolar x = Sum (Product x (Constant 2)) (Constant (-1))

unipolar :: Graph -> Graph
unipolar x = Sum (Product x (Constant 0.5)) (Constant 0.5)

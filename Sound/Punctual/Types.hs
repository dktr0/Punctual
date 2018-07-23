module Sound.Punctual.Types where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

import Sound.Punctual.Graph

-- Definitions (and transitions):
-- a <> sine 660 -- default crossfade
-- a <2s> sine 660 -- a 2 second crossfade, kind of like xFadein 4 in Tidal
-- a <3c> sine 880 -- a 3-cycle crossfade
-- a @2s <4s> sine 990 -- a 4 second crossfade, starting 2 seconds after eval time
-- a <10s>         -- a 10-second fade out
-- a ~ sine 1 -- when we change the definition of an LFO...
-- a ~ sine 0.5 -- ...we might want to preserve phase instead of crossfade
-- a @(4c,0.5c) ~ sine 0.25 -- 0.5 cycles after next 4-cycle boundary
-- a = sine 4 -- or, more rarely, we might want an instantaneous change
-- <2s> sine 440 -- target is anonymous
-- sine 440 -- target is anonymous and transition is default crossfade

data Duration = Seconds Double | Cycles Double deriving (Show,Eq)

duration :: GenParser Char a Duration
duration = choice $ fmap try [seconds,milliseconds,cycles]

seconds :: GenParser Char a Duration
seconds = do
  x <- fractional3 False
  char 's'
  return $ Seconds x

milliseconds :: GenParser Char a Duration
milliseconds = do
  x <- fractional3 False
  string "ms"
  return $ Seconds (x/1000.0)

cycles :: GenParser Char a Duration
cycles = do
  x <- fractional3 False
  char 'c'
  return $ Cycles x

data DefTime = After Duration | Quant Double Duration deriving (Show,Eq)

defTime :: GenParser Char a DefTime
defTime = choice $ fmap try [after,quant]

after :: GenParser Char a DefTime
after = spaces >> char '@' >> (After <$> duration)

quant :: GenParser Char a DefTime
quant = do
  spaces >> string "@("
  x <- fractional3 False
  spaces
  char ','
  spaces
  y <- duration
  spaces
  char ')'
  return $ Quant x y

data Transition = DefaultCrossFade | CrossFade Duration | HoldPhase deriving (Show, Eq)

transition :: GenParser Char a Transition
transition = choice [
  try (spaces >> string "<>" >> return DefaultCrossFade),
  try crossFade,
  try (spaces >> char '~' >> return HoldPhase),
  try (spaces >> char '=' >> return (CrossFade (Seconds 0.0)))
  ]

crossFade :: GenParser Char a Transition
crossFade = do
  spaces >> char '<' >> spaces
  x <- duration
  char '>'
  return $ CrossFade x

data Target = Explicit String | Anonymous deriving (Show,Eq)

data Definition = Definition Target DefTime Transition Graph deriving (Show, Eq)

definition :: GenParser Char a Definition
definition = choice [
  try targetDefTimeTransitionGraph,
  try targetTransitionGraph,
  try targetDefTimeGraph,
  try defTimeTransitionGraph,
  try defTimeGraph,
  try transitionGraph,
  Definition Anonymous (After (Seconds 0)) DefaultCrossFade <$> graph
  ]

explicitTarget :: GenParser Char a Target
explicitTarget = spaces >> (Explicit <$> many1 letter)

targetDefTimeTransitionGraph :: GenParser Char a Definition
targetDefTimeTransitionGraph = do
  t <- explicitTarget
  d <- defTime
  tr <- transition
  g <- graph
  return $ Definition t d tr g

targetTransitionGraph :: GenParser Char a Definition
targetTransitionGraph = do
  t <- explicitTarget
  tr <- transition
  g <- graph
  return $ Definition t (After (Seconds 0)) tr g

targetDefTimeGraph :: GenParser Char a Definition
targetDefTimeGraph = do
  t <- explicitTarget
  d <- defTime
  g <- graph
  return $ Definition t d DefaultCrossFade g

defTimeTransitionGraph :: GenParser Char a Definition
defTimeTransitionGraph = do
  d <- defTime
  tr <- transition
  g <- graph
  return $ Definition Anonymous d tr g

defTimeGraph :: GenParser Char a Definition
defTimeGraph = do
  x <- defTime
  spaces
  y <- graph
  return $ Definition Anonymous x DefaultCrossFade y

transitionGraph :: GenParser Char a Definition
transitionGraph = do
  x <- transition
  spaces
  y <- graph
  return $ Definition Anonymous (After (Seconds 0)) x y

-- routing things to the output (avoiding words like left or right to be less English)
-- sine 440 :0.5   -- centre panned
-- sine 440 :50%   -- the same thing another way
-- sine 440 :      -- the same thing yet another way
-- (sine 440 :square 80bpm -- dynamic panning, to be implemented later)
-- (sine 440 :a            -- dynamic panning according to "a")
-- a <> sine 440 :left -- something can be routed to a target and to an output

data Output = NoOutput | PannedOutput Extent deriving (Show,Eq)

output :: GenParser Char a Output
output = spaces >> choice [
  try (char ':' >> spaces >> extent >>= return . PannedOutput),
  try (char ':' >> return (PannedOutput 0.5)),
  return NoOutput
  ]

type Extent = Double

extent :: GenParser Char a Extent
extent = choice $ fmap try [extentDb,extentPercent,fractional3 False]

extentDb :: GenParser Char a Extent
extentDb = do
  x <- double
  spaces
  choice $ fmap (try . string) ["db","dB","Db","DB"]
  return $ dbamp x

extentPercent :: GenParser Char a Extent
extentPercent = do
  x <- fractional3 False
  spaces
  char '%'
  return $ x / 100

dbamp :: Double -> Double
dbamp x = 10 ** (x/20)

data Expression = Expression Definition Output deriving (Show,Eq)

expression :: GenParser Char a Expression
expression = Expression <$> definition <*> output

parsePunctual :: String -> Either ParseError Expression
parsePunctual = parse punctualParser "(unknown)"

punctualParser :: GenParser Char a Expression
punctualParser = spaces >> expression

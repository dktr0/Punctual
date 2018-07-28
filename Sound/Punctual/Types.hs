module Sound.Punctual.Types where

import Text.ParserCombinators.Parsec
import Sound.Punctual.Token
import Sound.Punctual.Extent
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

data DefTime = After Duration | Quant Double Duration deriving (Show,Eq)

defTime :: GenParser Char a DefTime
defTime = do
  reservedOp "@"
  (After <$> duration) <|> quant

quant :: GenParser Char a DefTime
quant = parens $ do
  x <- double
  comma
  y <- duration
  return $ Quant x y

data Transition = DefaultCrossFade | CrossFade Duration | HoldPhase deriving (Show, Eq)

transition :: GenParser Char a Transition
transition = choice [
  reservedOp "<>" >> return DefaultCrossFade,
  reservedOp "~" >> return HoldPhase,
  reservedOp "=" >> return (CrossFade (Seconds 0.0)),
  CrossFade <$> angles duration
  ]

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
  try targetGraph,
  Definition Anonymous (After (Seconds 0)) DefaultCrossFade <$> graph
  ]

explicitTarget :: GenParser Char a Target
explicitTarget = (Explicit <$> identifier)

targetDefTimeTransitionGraph :: GenParser Char a Definition
targetDefTimeTransitionGraph = Definition <$> explicitTarget <*> defTime <*> transition <*> graph

targetTransitionGraph :: GenParser Char a Definition
targetTransitionGraph = Definition <$> explicitTarget <*> dt <*> transition <*> graph
  where dt = return (After (Seconds 0))

targetDefTimeGraph :: GenParser Char a Definition
targetDefTimeGraph = Definition <$> explicitTarget <*> defTime <*> tr <*> graph
  where tr = return DefaultCrossFade

defTimeTransitionGraph :: GenParser Char a Definition
defTimeTransitionGraph = Definition <$> t <*> defTime <*> transition <*> graph
  where t = return Anonymous

defTimeGraph :: GenParser Char a Definition
defTimeGraph = Definition <$> t <*> defTime <*> tr <*> graph
  where
    t = return Anonymous
    tr = return DefaultCrossFade

transitionGraph :: GenParser Char a Definition
transitionGraph = Definition <$> t <*> dt <*> transition <*> graph
  where
    t = return Anonymous
    dt = return (After (Seconds 0))

targetGraph :: GenParser Char a Definition
targetGraph = Definition <$> explicitTarget <*> dt <*> tr <*> graph
  where
    dt = return (After (Seconds 0))
    tr = return DefaultCrossFade

-- routing things to the output (avoiding words like left or right to be less English)
-- sine 440 :0.5   -- centre panned
-- sine 440 :50%   -- the same thing another way
-- sine 440 :      -- the same thing yet another way
-- (sine 440 :square 80bpm -- dynamic panning, to be implemented later)
-- (sine 440 :a            -- dynamic panning according to "a")
-- a <> sine 440 :left -- something can be routed to a target and to an output

data Output = NoOutput | PannedOutput Extent deriving (Show,Eq)

output :: GenParser Char a Output
output = choice [
  try $ reservedOp ":" >> (PannedOutput <$> extent),
  reservedOp ":" >> return (PannedOutput 0.5),
  return NoOutput
  ]



data Expression = Expression Definition Output deriving (Show,Eq)

expression :: GenParser Char a Expression
expression = Expression <$> definition <*> output

punctualParser :: GenParser Char a Expression
punctualParser = do
  x <- whiteSpace >> expression
  eof
  return x

runPunctualParser :: String -> Either ParseError Expression
runPunctualParser = parse punctualParser ""

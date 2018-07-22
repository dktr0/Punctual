module Sound.Punctual.Types where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

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
-- data Definition = Definition Target DefTime Transition Graph deriving (Show, Eq)

{-
definition :: GenParser Char a Definition
definition = choice [
  try (graph >>= ),
  try ()
  ]
-}

parsePunctual :: String -> Either ParseError DefTime
parsePunctual = parse punctualParser "(unknown)"

punctualParser :: GenParser Char a DefTime
punctualParser = spaces >> defTime

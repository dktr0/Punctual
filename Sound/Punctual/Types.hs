module Sound.Punctual.Types where

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

data DefTime = After Duration | Quant Double Duration deriving (Show,Eq)

data Transition = DefaultCrossFade | CrossFade Duration | HoldPhase deriving (Show, Eq)

data Target = Explicit String | Anonymous deriving (Show,Eq)

data Definition = Definition {
  target :: Target,
  defTime :: DefTime,
  transition :: Transition,
  graph :: Graph
  } deriving (Show, Eq)

-- routing things to the output (avoiding words like left or right to be less English)
-- sine 440 :0.5   -- centre panned
-- sine 440 :50%   -- the same thing another way
-- sine 440 :      -- the same thing yet another way
-- (sine 440 :square 80bpm -- dynamic panning, to be implemented later)
-- (sine 440 :a            -- dynamic panning according to "a")
-- a <> sine 440 :left -- something can be routed to a target and to an output

data Output = NoOutput | PannedOutput Extent deriving (Show,Eq)

data Expression = Expression {
  definition :: Definition,
  output :: Output
  } deriving (Show,Eq)

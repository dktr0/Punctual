module Sound.Punctual.Types where

import Sound.Punctual.Token
import Sound.Punctual.Extent
import Sound.Punctual.Graph

data Duration = Seconds Double | Cycles Double deriving (Show,Eq)

data DefTime = After Duration | Quant Double Duration deriving (Show,Eq)

data Transition = DefaultCrossFade | CrossFade Duration | HoldPhase deriving (Show, Eq)

data Target = Explicit String | Anonymous deriving (Show,Eq,Ord)

data Definition = Definition {
  target :: Target,
  defTime :: DefTime,
  transition :: Transition,
  graph :: Graph
  } deriving (Show, Eq)

definitionIsExplicitlyNamed :: Definition -> Bool
definitionIsExplicitlyNamed (Definition (Explicit _) _ _ _) = True
definitionIsExplicitlyNamed _ = False

explicitTargetOfDefinition :: Definition -> String
explicitTargetOfDefinition (Definition (Explicit x) _ _ _) = x
explicitTargetOfDefinition _ = ""

-- sine 440 => 0.5   -- centre panned
-- sine 440 => 50%   -- the same thing another way
-- sine 440 =>      -- the same thing yet another way
-- a <> sine 440 => left -- something can be routed to a target and to an output

data Output = NoOutput | PannedOutput Extent | NamedOutput String deriving (Show,Eq)

data Expression = Expression {
  definition :: Definition,
  output :: Output
  } deriving (Show,Eq)

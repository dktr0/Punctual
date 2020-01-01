{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Sound.Punctual.Types where

import Data.Time
import Data.Map.Strict
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq

import Sound.Punctual.Graph

type AudioTime = Double

type Extent = Double

data Duration = Seconds Double | Cycles Double deriving (Show,Eq,Generic,NFData)

data DefTime = After Duration | Quant Double Duration deriving (Show,Eq,Generic,NFData)

calculateT1 :: (AudioTime,Double) -> AudioTime -> DefTime -> AudioTime
calculateT1 _ evalTime (After (Seconds t)) = t + evalTime
calculateT1 (_,cps) evalTime (After (Cycles t)) = (t/(realToFrac cps)) + evalTime
calculateT1 (t0,cps) evalTime (Quant n (Seconds t)) = t + nextBoundary
  where
    sinceBeat0 = evalTime - t0
    minimumT1inQuants = sinceBeat0 * (realToFrac $ cps/n)
    beat0toBoundary = fromIntegral (floor minimumT1inQuants + 1 :: Integer) * (realToFrac $ n/cps)
    nextBoundary = beat0toBoundary + t0
calculateT1 (t0,cps) evalTime (Quant n (Cycles t)) = (t/(realToFrac cps)) + nextBoundary
  where
    sinceBeat0 = evalTime - t0
    minimumT1inQuants = sinceBeat0 * (realToFrac $ cps/n)
    beat0toBoundary = fromIntegral (floor minimumT1inQuants + 1 :: Integer) * (realToFrac $ n/cps)
    nextBoundary = beat0toBoundary + t0

data Transition = DefaultCrossFade | CrossFade Duration | HoldPhase deriving (Show, Eq, Generic, NFData)

-- note: returned value represents half of total xfade duration
transitionToXfade :: Double -> Transition -> AudioTime
transitionToXfade _ DefaultCrossFade = 0.25
transitionToXfade _ (CrossFade (Seconds x)) = realToFrac x
transitionToXfade cps (CrossFade (Cycles x)) = (realToFrac $ x/cps)
transitionToXfade _ HoldPhase = 0.005

data Target = Explicit Text | Anonymous deriving (Show,Eq,Ord,Generic,NFData)

-- *** the type Target' is like Target except that anonymous targets have a numerical index
-- In the future we should replace Target with Target' (and expect the parser to provide a valid index)

data Target' = Named Text | Anon Int deriving (Show,Eq,Ord,Generic,NFData)

data Definition = Definition {
  target :: Target,
  defTime :: DefTime,
  transition :: Transition,
  graph :: Graph
  } deriving (Show, Eq, Generic, NFData)

definitionIsExplicitlyNamed :: Definition -> Bool
definitionIsExplicitlyNamed (Definition (Explicit _) _ _ _) = True
definitionIsExplicitlyNamed _ = False

explicitTargetOfDefinition :: Definition -> Text
explicitTargetOfDefinition (Definition (Explicit x) _ _ _) = x
explicitTargetOfDefinition _ = ""

data Output = NoOutput | PannedOutput Extent | NamedOutput Text deriving (Show,Eq,Generic,NFData)

data Expression = Expression {
  definition :: Definition,
  output :: Output
  } deriving (Show,Eq,Generic,NFData)

expressionFromGraph :: Graph -> Expression
expressionFromGraph g = Expression {
  definition = Definition {
    target = Anonymous,
    defTime = Quant 1.0 (Seconds 0.0),
    transition = DefaultCrossFade,
    graph = g
    },
  output = NoOutput
  }

(<>) :: Expression -> Duration -> Expression
e <> d = e {
  definition = (definition e) {
    transition = CrossFade d
    }
  }

(@@) :: Expression -> DefTime -> Expression
e @@ d = e {
  definition = (definition e) {
    defTime = d
    }
  }

(>>) :: Expression -> Output -> Expression
e >> o = e { output = o }


listOfExpressionsToMap :: [Expression] -> Map Target' Expression
listOfExpressionsToMap xs = fromList $ namedExprs ++ anonymousExprs
  where
    namedExprs = fmap (\e -> (Named $ explicitTargetOfDefinition $ definition e,e)) $ Prelude.filter (definitionIsExplicitlyNamed . definition) xs
    anonymousExprs = zipWith (\e n -> (Anon n,e)) (Prelude.filter ((not . definitionIsExplicitlyNamed) . definition) xs) [0..]

expressionToTimes :: (AudioTime,Double) -> AudioTime -> Expression -> (AudioTime,AudioTime)
expressionToTimes tempo evalTime x = definitionToTimes tempo evalTime (definition x)

definitionToTimes :: (AudioTime,Double) -> AudioTime -> Definition -> (AudioTime,AudioTime)
definitionToTimes tempo evalTime x = defTimeAndTransitionToTimes tempo evalTime (defTime x) (transition x)

defTimeAndTransitionToTimes :: (AudioTime,Double) -> AudioTime -> DefTime -> Transition -> (AudioTime,AudioTime)
defTimeAndTransitionToTimes tempo@(_,cps) evalTime dt tr = (t1,t2)
  where
    t1 = calculateT1 tempo evalTime dt
    t2 = (transitionToXfade cps tr) + t1

data Program = Program {
  directGLSL :: Maybe Text,
  expressions :: [Expression]
}

emptyProgram :: Program
emptyProgram = Program {
  directGLSL = Nothing,
  expressions = []
}

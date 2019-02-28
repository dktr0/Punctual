module Sound.Punctual.Types where

import Data.Time
import Data.Map.Strict

import Sound.Punctual.Token
import Sound.Punctual.Extent
import Sound.Punctual.Graph

data Duration = Seconds Double | Cycles Double deriving (Show,Eq)

data DefTime = After Duration | Quant Double Duration deriving (Show,Eq)

calculateT1 :: (UTCTime,Double) -> UTCTime -> DefTime -> UTCTime
calculateT1 _ evalTime (After (Seconds t)) = addUTCTime (realToFrac t) evalTime
calculateT1 (_,cps) evalTime (After (Cycles t)) = addUTCTime (realToFrac $ t/cps) evalTime
calculateT1 (t0,cps) evalTime (Quant n (Seconds t)) = addUTCTime (realToFrac t) nextBoundary
  where
    sinceBeat0 = diffUTCTime evalTime t0
    minimumT1inQuants = sinceBeat0 * (realToFrac $ cps/n)
    beat0toBoundary = fromIntegral (floor minimumT1inQuants + 1) * (realToFrac $ n/cps)
    nextBoundary = addUTCTime beat0toBoundary t0
calculateT1 (t0,cps) evalTime (Quant n (Cycles t)) = addUTCTime (realToFrac $ t/cps)  nextBoundary
  where
    sinceBeat0 = diffUTCTime evalTime t0
    minimumT1inQuants = sinceBeat0 * (realToFrac $ cps/n)
    beat0toBoundary = fromIntegral (floor minimumT1inQuants + 1) * (realToFrac $ n/cps)
    nextBoundary = addUTCTime beat0toBoundary t0

data Transition = DefaultCrossFade | CrossFade Duration | HoldPhase deriving (Show, Eq)

-- note: returned value represents half of total xfade duration
transitionToXfade :: Double -> Transition -> NominalDiffTime
transitionToXfade _ DefaultCrossFade = 0.25
transitionToXfade _ (CrossFade (Seconds x)) = realToFrac x
transitionToXfade cps (CrossFade (Cycles x)) = (realToFrac $ x/cps)
transitionToXfade _ HoldPhase = 0.005

data Target = Explicit String | Anonymous deriving (Show,Eq,Ord)

-- *** the type Target' is like Target except that anonymous targets have a numerical index
-- In the future we should replace Target with Target' (and expect the parser to provide a valid index)

data Target' = Named String | Anon Int deriving (Show,Eq,Ord)

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

listOfExpressionsToMap :: [Expression] -> Map Target' Expression
listOfExpressionsToMap xs = fromList $ namedExprs ++ anonymousExprs
  where
    namedExprs = fmap (\e -> (Named $ explicitTargetOfDefinition $ definition e,e)) $ Prelude.filter (definitionIsExplicitlyNamed . definition) xs
    anonymousExprs = zipWith (\e n -> (Anon n,e)) (Prelude.filter ((not . definitionIsExplicitlyNamed) . definition) xs) [0..]

expressionToTimes :: (UTCTime,Double) -> UTCTime -> Expression -> (UTCTime,UTCTime)
expressionToTimes tempo evalTime x = definitionToTimes tempo evalTime (definition x)

definitionToTimes :: (UTCTime,Double) -> UTCTime -> Definition -> (UTCTime,UTCTime)
definitionToTimes tempo evalTime x = defTimeAndTransitionToTimes tempo evalTime (defTime x) (transition x)

defTimeAndTransitionToTimes :: (UTCTime,Double) -> UTCTime -> DefTime -> Transition -> (UTCTime,UTCTime)
defTimeAndTransitionToTimes tempo@(_,cps) evalTime dt tr = (t1,t2)
  where
    t1 = calculateT1 tempo evalTime dt
    t2 = addUTCTime (transitionToXfade cps tr) t1

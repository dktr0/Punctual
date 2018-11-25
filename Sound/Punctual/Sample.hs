module Sound.Punctual.Sample where

import Data.Time.Clock
import Data.List (find)

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Sound.Punctual.Evaluation

findGraphForTarget :: String -> PunctualState -> Maybe Graph
findGraphForTarget targetName s = fmap (graph . definition) $ find ((==Explicit targetName) . target . definition) $ expressions s

sampleWithDefault :: String -> Double -> PunctualState -> UTCTime -> Double
sampleWithDefault targetName d s t = maybe d id $ fmap (sampleGraph (startTime s) t 0) $ findGraphForTarget targetName s

samplePunctualState :: PunctualState -> UTCTime -> Int -> Double
samplePunctualState s t channel = sum $ fmap (sampleExpression (startTime s) t channel) $ expressions s

-- eTime is a placeholder, declaring the supposed start time of the expression
-- (ultimately, each expression could have a different start time as a result of entire evaluation history)
sampleExpression :: UTCTime -> UTCTime -> Int -> Expression -> Double
sampleExpression eTime sampleTime channel e | output e == NoOutput = 0.0
sampleExpression eTime sampleTime channel e | otherwise = sampleDefinition eTime sampleTime channel (definition e)

sampleDefinition :: UTCTime -> UTCTime -> Int -> Definition -> Double
sampleDefinition dTime sampleTime channel d = sampleGraph dTime sampleTime channel (graph d)

sampleGraph :: UTCTime -> UTCTime -> Int -> Graph -> Double
sampleGraph t1 t2 channel g = sampleGraph' (diffUTCTime t2 t1) channel g

fromDiffTime :: NominalDiffTime -> Double
fromDiffTime = fromRational . toRational

sampleGraph' :: NominalDiffTime -> Int -> Graph -> Double
sampleGraph' _ _ EmptyGraph = 0.0
sampleGraph' _ _ (Constant x) = x
sampleGraph' _ _ (Noise) = 0.5 -- placeholder
sampleGraph' _ _ (Pink) = 0.5 -- placeholder
sampleGraph' t c (Sine x) = sin $ fromDiffTime t * sampleGraph' t c x * 2 * pi
sampleGraph' t c (Tri x) = tri $ fromDiffTime t * sampleGraph' t c x
sampleGraph' t c (Saw x) = saw $ fromDiffTime t * sampleGraph' t c x
sampleGraph' t c (Square x) = sqr $ fromDiffTime t * sampleGraph' t c x
sampleGraph' t c (LPF x _ _) = sampleGraph' t c x -- placeholder
sampleGraph' t c (HPF x _ _) = sampleGraph' t c x -- placeholder
sampleGraph' t _ (FromTarget _) = 1.0 -- placeholder
sampleGraph' t c (Product x y) = sampleGraph' t c x * sampleGraph' t c y
sampleGraph' t c (Sum x y) = sampleGraph' t c x + sampleGraph' t c y

tri :: Double -> Double
tri y = f $ (\x -> x - fromIntegral (floor x)) $ y + 0.25
  where
    f x | x < 0.5 = (x * 4) - 1
    f x | otherwise = 1 - ((x - 0.5) * 4)

saw :: Double -> Double
saw x = x - fromIntegral (floor x)

sqr :: Double -> Double
sqr x = f $ x - fromIntegral (floor x)
  where
    f x | x < 0.5 = 1
    f x | otherwise = -1

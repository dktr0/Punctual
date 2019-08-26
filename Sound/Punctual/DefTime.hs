module Sound.Punctual.DefTime where

import Sound.Punctual.AudioTime
import Sound.Punctual.Duration

data DefTime = After Duration | Quant Double Duration deriving (Show,Eq)

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

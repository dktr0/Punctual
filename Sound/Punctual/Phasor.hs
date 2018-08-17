module Sound.Punctual.Phasor where

import Data.Time.Clock

data Phasor = Phasor {
  phasorF :: Double,
  phasorT :: UTCTime
  }

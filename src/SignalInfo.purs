module SignalInfo where

import Prelude (mempty, pure)
import Data.Set (Set)
import Data.Monoid.Disj (Disj)

type SignalInfo = {
  webcam :: Disj Boolean,
  fft :: Disj Boolean,
  lo :: Disj Boolean,
  mid :: Disj Boolean,
  hi :: Disj Boolean,
  ifft :: Disj Boolean,
  ilo :: Disj Boolean,
  imid :: Disj Boolean,
  ihi :: Disj Boolean,
  ain :: Disj Boolean,
  imgURLs :: Set String,
  vidURLs :: Set String,
  gdmIDs :: Set String
  }

emptySignalInfo :: SignalInfo
emptySignalInfo = {
  webcam: pure false,
  fft: pure false,
  lo: pure false,
  mid: pure false,
  hi: pure false,
  ifft: pure false,
  ilo: pure false,
  imid: pure false,
  ihi: pure false,
  ain: pure false,
  imgURLs: mempty,
  vidURLs: mempty,
  gdmIDs: mempty
  }
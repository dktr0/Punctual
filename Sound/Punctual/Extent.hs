module Sound.Punctual.Extent where

type Extent = Double

dbamp :: Double -> Double
dbamp x = 10 ** (x/20)

midicps :: Double -> Double
midicps x = 440 * (2**((x-69)/12))

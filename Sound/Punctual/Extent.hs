module Sound.Punctual.Extent where

type Extent = Double

dbamp :: Double -> Double
dbamp x = 10.0 ** (x / 20.0)

ampdb :: Double -> Double
ampdb x = 20.0 * (logBase 10 x)

midicps :: Double -> Double
midicps x = 440.0 * (2.0 ** ((x - 69.0) / 12.0))

cpsmidi :: Double -> Double
cpsmidi x = 69.0 + 12.0 * (logBase 2 (x / 440.0))

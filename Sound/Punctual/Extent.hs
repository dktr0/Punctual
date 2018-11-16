module Sound.Punctual.Extent where

import Text.ParserCombinators.Parsec
import Sound.Punctual.Token

type Extent = Double

extent :: GenParser Char a Extent
extent = choice $ fmap try [extentDb,extentPercent,extentMidi,double]

extentDb :: GenParser Char a Extent
extentDb = do
  x <- double
  reserved "db"
  return $ dbamp x

extentPercent :: GenParser Char a Extent
extentPercent = do
  x <- double
  reservedOp "%"
  return $ x / 100

extentMidi :: GenParser Char a Extent
extentMidi = do
  x <- double
  reserved "m"
  return $ midicps x

dbamp :: Double -> Double
dbamp x = 10 ** (x/20)

midicps :: Double -> Double
midicps x = 440 * (2**((x-69)/12))

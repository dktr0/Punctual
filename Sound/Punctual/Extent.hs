module Sound.Punctual.Extent where

import Text.Parsec
import Text.Parsec.Text
import Sound.Punctual.Token

type Extent = Double

extent :: Parser Extent
extent = choice $ fmap try [extentDb,extentPercent,extentMidi,double]

extentDb :: Parser Extent
extentDb = do
  x <- double
  reserved "db"
  return $ dbamp x

extentPercent :: Parser Extent
extentPercent = do
  x <- double
  reservedOp "%"
  return $ x / 100

extentMidi :: Parser Extent
extentMidi = do
  x <- double
  reserved "m"
  return $ midicps x

dbamp :: Double -> Double
dbamp x = 10 ** (x/20)

midicps :: Double -> Double
midicps x = 440 * (2**((x-69)/12))

module Sound.Punctual.Extent where

import Text.ParserCombinators.Parsec
import Sound.Punctual.Token

type Extent = Double

extent :: GenParser Char a Extent
extent = choice $ fmap try [extentDb,extentPercent,double]

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

dbamp :: Double -> Double
dbamp x = 10 ** (x/20)

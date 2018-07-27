module Sound.Punctual.Tests where

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Text.ParserCombinators.Parsec
import Test.HUnit

t :: String -> Either ParseError Expression -> Test
t x y = x ~: (runPunctualParser x) ~?= y

tests = TestList [
  t "" (Right (Expression (Definition Anonymous (After (Seconds 0.0)) DefaultCrossFade EmptyGraph) NoOutput)),
  t "a <> b + c" (Right (Expression (Definition (Explicit "a ") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "b ") (FromTarget "c "))) NoOutput)),
  t "a <> b + c:" (Right (Expression (Definition (Explicit "a ") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "b ") (FromTarget "c "))) (PannedOutput 0.5)))
  ]

doTests = runTestTT tests

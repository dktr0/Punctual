module Sound.Punctual.Tests where

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Text.ParserCombinators.Parsec
import Test.HUnit

t :: String -> Either ParseError Expression -> Test
t x y = x ~: (runPunctualParser x) ~?= y

tests = TestList [
  t "" (Right (Expression (Definition Anonymous (After (Seconds 0.0)) DefaultCrossFade EmptyGraph) NoOutput)),
  t "x<>y+z" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) NoOutput)),
  t "x <> y + z " (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) NoOutput)),
  t "x <> sine 440 + sine 440" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (Sine (Constant 440.0)) (Sine (Constant 440.0)))) NoOutput)),
  t "x<>y+z:" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.5))),
  t "x<>y+z:0.2" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.2))),
  t "x<>y+z:30%" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.3))),
  t "x<>y+z:-3db" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.7079457843841379))),
  t "a @3s ~ sine (sine 440 * -3db )" (Right (Expression (Definition (Explicit "a") (After (Seconds 3.0)) HoldPhase (Sine (Product (Sine (Constant 440.0)) (Constant 0.7079457843841379)))) NoOutput))
  ]

doTests = runTestTT tests

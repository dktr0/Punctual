module Sound.Punctual.Tests where

import Sound.Punctual.Graph
import Sound.Punctual.Types
import Sound.Punctual.Parser
import Text.ParserCombinators.Parsec
import Test.HUnit

t :: String -> Either ParseError Expression -> Test
t x y = x ~: (runPunctualParser x) ~?= (fmap (\z -> [z]) y)

tests = TestList [
  t "" (Right (Expression (Definition Anonymous (After (Seconds 0.0)) DefaultCrossFade EmptyGraph) NoOutput)),
  t "x<>y+z" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) NoOutput)),
  t "x <> y + z " (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) NoOutput)),
  t "x <> sine 440 + sine 440" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (Sine (Constant 440.0)) (Sine (Constant 440.0)))) NoOutput)),
  t "x<>y+z:" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.5))),
  t "x<>y+z:0.2" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.2))),
  t "x<>y+z:30%" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.3))),
  t "x<>y+z:-3db" (Right (Expression (Definition (Explicit "x") (After (Seconds 0.0)) DefaultCrossFade (Sum (FromTarget "y") (FromTarget "z"))) (PannedOutput 0.7079457843841379))),
  t "a @3s ~ sine (sine 440 * -3db )" (Right (Expression (Definition (Explicit "a") (After (Seconds 3.0)) HoldPhase (Sine (Product (Sine (Constant 440.0)) (Constant 0.7079457843841379)))) NoOutput)),
  t "@3s square 110" (Right (Expression (Definition Anonymous (After (Seconds 3.0)) DefaultCrossFade (Square (Constant 110.0))) NoOutput)),
  t "a sine 440 :" (Right (Expression (Definition (Explicit "a") (After (Seconds 0.0)) DefaultCrossFade (Sine (Constant 440.0))) (PannedOutput 0.5))),
  t " noise + pink " (Right (Expression (Definition Anonymous (After (Seconds 0.0)) DefaultCrossFade (Sum Noise Pink)) NoOutput)),
  t "noise+pink*-12db" (Right (Expression (Definition Anonymous (After (Seconds 0.0)) DefaultCrossFade (Sum Noise (Product Pink (Constant 0.251188643150958)))) NoOutput))
  ]

doTests = runTestTT tests

module Sound.Punctual.Test where

import Data.Time
import Data.Tempo
import Data.Map as Map
import Data.IntMap as IntMap
import Data.Text
import Data.Text.IO as T

import Sound.Punctual.Program
import Sound.Punctual.Parser
import Sound.Punctual.FragmentShader

testFragmentShader :: Text -> IO ()
testFragmentShader x = do
  now <- getCurrentTime
  case parse now x of
    Left err -> Prelude.putStrLn $ "parse error: " ++ err
    Right p -> do
      Prelude.putStrLn ""
      Prelude.putStrLn $ show p
      Prelude.putStrLn ""
      let testTempo = Tempo 1.0 now 0
      T.putStrLn $ fragmentShader testTempo Map.empty (emptyProgram now) p

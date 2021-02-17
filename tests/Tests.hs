{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec
-- import Data.Set as Set
import Data.IntMap.Strict as IntMap
import Data.Time

import Sound.Punctual.Parser
import Sound.Punctual.Program
import Sound.Punctual.Action
import Sound.Punctual.Graph
import Sound.Punctual.Transition
import Sound.Punctual.DefTime
import Sound.Punctual.Duration
import Sound.Punctual.Output

main :: IO ()
main = do
  now <- getCurrentTime
  microspec $ do

    describe "the parser parses empty programs from " $ do
      let emptyPrograms = Right $ emptyProgram now
      it "the empty string" $ parse now "" `shouldBe` emptyPrograms
      it "just spaces" $ parse now "     " `shouldBe` emptyPrograms
      it "a mix of tabs, newlines, and spaces" $ parse now " \t\n   \t\t\t\t\n    \t" `shouldBe` emptyPrograms
      it "a semi-colon" $ parse now ";" `shouldBe` emptyPrograms
      it "two semi-colons" $ parse now ";;" `shouldBe` emptyPrograms
      it "a one-line comment" $ parse now "-- this is a comment" `shouldBe` emptyPrograms
      it "a one-line comment with a semicolon" $ parse now "-- this is a comment;" `shouldBe` emptyPrograms
      it "a one-line comment with two actions separated by a semicolon" $ parse now "-- circle 0 0.25 >> rgb; vline 0 0.002 >> rgb" `shouldBe` emptyPrograms
      it "just two one-line comments" $ parse now "-- comment\n--another comment" `shouldBe` emptyPrograms
      it "just a multi-line comment" $ parse now "{- this is a\n comment-}" `shouldBe` emptyPrograms
      it "just a multi-line comment with a semicolon" $ parse now "{- this is a;\n comment-}" `shouldBe` emptyPrograms
      it "a non-output 0" $ parse now "0" `shouldBe` emptyPrograms
      it "a non-output 0 and a one-line comment" $ parse now "0 -- comment" `shouldBe` emptyPrograms
      it "a non-output 0 and a multi-line comment" $ parse now "0 {- comment\n-}" `shouldBe` emptyPrograms

    describe "the parse parses simple programs, eg." $ do
      let x = emptyProgram now
      it "a simple sine wave to splay" $ parse now "sin 440 >> splay" `shouldBe` Right (x{ actions = IntMap.fromList [(0,Action {graph = Sin (Constant 440.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

      it "a simple sine wave to splay, with a final semicolon" $ parse now "sin 440 >> splay;" `shouldBe` Right (x { actions = IntMap.fromList [(0,Action {graph = Sin (Constant 440.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

      it "two sine waves to splay, with no final semicolon" $ parse now "sin 440 >> splay;\n sin 550 >> splay" `shouldBe` Right (x { actions = IntMap.fromList [(0,Action {graph = Sin (Constant 440.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]}),(1,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

      it "two sine waves to splay, with a final semicolon" $ parse now "sin 440 >> splay;\n sin 550 >> splay;" `shouldBe` Right (x { actions = IntMap.fromList [(0,Action {graph = Sin (Constant 440.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]}),(1,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

      it "two sine waves to splay, but the first commented out" $ parse now "-- sin 440 >> splay;\n sin 550 >> splay" `shouldBe` Right (x {actions = IntMap.fromList [(0,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

      it "two sine waves to splay, the first commented out with a final semicolon" $ parse now "-- sin 440 >> splay;\n sin 550 >> splay;" `shouldBe` Right (x {actions = IntMap.fromList [(0,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

      it "a simple circle, with no final semicolon" $ parse now "circle 0 0.25 >> rgb"  `shouldBe` Right (x { actions = IntMap.fromList [(0,Action {graph = Circle (Constant 0.0) (Constant 0.25), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [RGB]})]})

      it "a simple circle, with a final semicolon" $ parse now "circle 0 0.25 >> rgb;"  `shouldBe` Right (x { actions = IntMap.fromList [(0,Action {graph = Circle (Constant 0.0) (Constant 0.25), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [RGB]})]})

      it "a simple circle, with two final semicolons" $ parse now "circle 0 0.25 >> rgb;;"  `shouldBe` Right (x { actions = IntMap.fromList [(0,Action {graph = Circle (Constant 0.0) (Constant 0.25), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [RGB]})]})

      it "a circle, with a transition time" $ parse now "circle 0 0.25 >> rgb <> 5" `shouldBe` Right (x { actions = fromList [(0,Action {graph = Circle (Constant 0.0) (Constant 0.25), defTime = Quant 1.0 (Seconds 0.0), transition = CrossFade (Seconds 5.0), outputs = [RGB]})]})

      it "a circle, with a transition time (and a semi-colon)" $ parse now "circle 0 0.25 >> rgb <> 5;" `shouldBe` Right (x { actions = fromList [(0,Action {graph = Circle (Constant 0.0) (Constant 0.25), defTime = Quant 1.0 (Seconds 0.0), transition = CrossFade (Seconds 5.0), outputs = [RGB]})]})

      it "a circle, with a definition time" $ parse now "circle 0 0.25 >> rgb @@ 5" `shouldBe` Right (x { actions = fromList [(0,Action {graph = Circle (Constant 0.0) (Constant 0.25), defTime = After (Seconds 5.0), transition = DefaultCrossFade, outputs = [RGB]})]})

      it "a circle, with a definition time (and a semi-colon)" $ parse now "circle 0 0.25 >> rgb @@ 5" `shouldBe` Right (x { actions = fromList [(0,Action {graph = Circle (Constant 0.0) (Constant 0.25), defTime = After (Seconds 5.0), transition = DefaultCrossFade, outputs = [RGB]})]})

      it "zero assigned to a variable, routed to RGB output" $ parse now "t << 0; t >> rgb" `shouldBe` Right (x { actions = fromList [(0,Action {graph = Constant 0.0, defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [RGB]})]})

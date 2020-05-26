{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec
-- import Data.Set as Set
import Data.IntMap.Strict as IntMap
import Sound.Punctual.Parser
import Sound.Punctual.Program
import Sound.Punctual.Action
import Sound.Punctual.Graph
import Sound.Punctual.Transition
import Sound.Punctual.DefTime
import Sound.Punctual.Duration
import Sound.Punctual.Output

main :: IO ()
main = microspec $ do

  describe "the parser parses empty programs from " $ do
    let emptyPrograms = Right emptyProgram
    it "the empty string" $ parse 0.0 "" `shouldBe` emptyPrograms
    it "just spaces" $ parse 0.0 "     " `shouldBe` emptyPrograms
    it "a mix of tabs, newlines, and spaces" $ parse 0.0 " \t\n   \t\t\t\t\n    \t" `shouldBe` emptyPrograms
    it "just a semi-colon" $ parse 0.0 ";" `shouldBe` emptyPrograms
    it "just two semi-colons" $ parse 0.0 ";;" `shouldBe` emptyPrograms
    it "just a one-line comment" $ parse 0.0 "-- this is a comment" `shouldBe` emptyPrograms
    it "just a one-line comment with a semicolon" $ parse 0.0 "-- this is a comment;" `shouldBe` emptyPrograms
    it "just two one-line comments" $ parse 0.0 "-- comment\n--another comment" `shouldBe` emptyPrograms
    it "just a multi-line comment" $ parse 0.0 "{- this is a\n comment-}" `shouldBe` emptyPrograms
    it "just a multi-line comment with a semicolon" $ parse 0.0 "{- this is a;\n comment-}" `shouldBe` emptyPrograms

  describe "the parse parses simple programs, eg." $ do

    it "a silent 0" $ parse 0.0 "0" `shouldBe` Right (emptyProgram { actions = IntMap.fromList [(0,Action { graph = Constant 0.0, defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = []})]})

    it "a silent 0 and a one-line comment" $ parse 0.0 "0 -- comment" `shouldBe` Right (emptyProgram { actions = IntMap.fromList [(0,Action { graph = Constant 0.0, defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = []})]})

    it "a program containing just a silent 0 and a multi-line comment" $ parse 0.0 "0 {- comment\n-}" `shouldBe` Right (emptyProgram { actions = IntMap.fromList [(0,Action { graph = Constant 0.0, defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = []})]})

    it "a simple sine wave to splay" $ parse 0.0 "sin 440 >> splay" `shouldBe` Right (emptyProgram { actions = IntMap.fromList [(0,Action {graph = Sin (Constant 440.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

    it "two sine waves to splay, with no final semicolon" $ parse 0.0 "sin 440 >> splay;\n sin 550 >> splay" `shouldBe` Right (emptyProgram { actions = IntMap.fromList [(0,Action {graph = Sin (Constant 440.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]}),(1,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

    it "two sine waves to splay, with a final semicolon" $ parse 0.0 "sin 440 >> splay;\n sin 550 >> splay;" `shouldBe` Right (emptyProgram { actions = IntMap.fromList [(0,Action {graph = Sin (Constant 440.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]}),(1,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

    it "two sine waves to splay, but the first commented out" $ parse 0.0 "-- sin 440 >> splay;\n sin 550 >> splay" `shouldBe` Right (emptyProgram {actions = IntMap.fromList [(0,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

    it "two sine waves to splay, the first commented out with a final semicolon" $ parse 0.0 "-- sin 440 >> splay;\n sin 550 >> splay;" `shouldBe` Right (emptyProgram {actions = IntMap.fromList [(0,Action {graph = Sin (Constant 550.0), defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, outputs = [Splay]})]})

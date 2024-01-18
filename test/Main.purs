module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe,it,SpecT)
import Test.Spec.Assertions (shouldReturn)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.List (head)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Exception (Error,error)
import Parsing (ParseError)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Newtype (unwrap)

import Parser (testAST)
import Signal (Signal(..))
import Program (programInfo)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "signal of first action in simple programs is parsed correctly" do
    "0 >> video" `firstActionSignalIs` Constant 0.0
    "0.5 >> audio" `firstActionSignalIs` Constant 0.5
    "sin 440 >> audio" `firstActionSignalIs` (Sin (Constant 440.0))
  describe "needsWebCam flag in ProgramInfo is true/false as appropriate" do
    "sin 440 >> video" `needsWebCam` false
    "cam >> video" `needsWebCam` true
    "f x = cam * sin 440; 0 >> audio" `needsWebCam` false
    "f x = cam * sin 440; f 1 >> audio" `needsWebCam` true
  describe "needsAudioInputAnalysis flag in ProgramInfo is true/false as appropriate" do
    "sin 440 >> video" `needsAudioInputAnalysis` false
    "sin 440 * ilo >> video" `needsAudioInputAnalysis` true
    "sin 440 * imid >> video" `needsAudioInputAnalysis` true
    "sin 440 * ihi>> video" `needsAudioInputAnalysis` true
    "sin 440 * ifft fx >> video" `needsAudioInputAnalysis` true
  describe "needsAudioOutputAnalysis flag in ProgramInfo is true/false as appropriate" do
    "sin 440 >> video" `needsAudioOutputAnalysis` false
    "sin 440 * lo >> video" `needsAudioOutputAnalysis` true
    "sin 440 * mid >> video" `needsAudioOutputAnalysis` true
    "sin 440 * hi>> video" `needsAudioOutputAnalysis` true
    "sin 440 * fft fx >> video" `needsAudioOutputAnalysis` true



firstActionSignalIs :: forall g m. Monad m => MonadThrow Error g => String -> Signal -> SpecT g Unit m Unit
firstActionSignalIs txt sig = it txt $ firstActionSignal txt `shouldReturn` sig
  
needsWebCam :: forall m g. Monad m => MonadThrow Error g => String -> Boolean -> SpecT g Unit m Unit
needsWebCam txt b = it txt $ ((_.needsWebCam <<< unwrap <<< programInfo) <$> parseErrorToError (testAST txt)) `shouldReturn` b

needsAudioInputAnalysis :: forall m g. Monad m => MonadThrow Error g => String -> Boolean -> SpecT g Unit m Unit
needsAudioInputAnalysis txt b = it txt $ ((_.needsAudioInputAnalysis <<< unwrap <<< programInfo) <$> parseErrorToError (testAST txt)) `shouldReturn` b

needsAudioOutputAnalysis :: forall m g. Monad m => MonadThrow Error g => String -> Boolean -> SpecT g Unit m Unit
needsAudioOutputAnalysis txt b = it txt $ ((_.needsAudioOutputAnalysis <<< unwrap <<< programInfo) <$> parseErrorToError (testAST txt)) `shouldReturn` b

parseErrorToError :: forall m a. MonadThrow Error m => Either ParseError a -> m a
parseErrorToError x = do
  case x of
    Right r -> pure r
    Left pErr -> throwError $ error (show pErr) 

firstActionSignal :: forall m. MonadThrow Error m => String -> m Signal
firstActionSignal txt = do
  x <- parseErrorToError $ testAST txt
  case head x.actions of
    Just h -> do
       case h of
         Just a -> pure a.signal
         Nothing -> throwError $ error "no action"
    Nothing -> throwError $ error "no actions"


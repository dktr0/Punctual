module AudioWorklet where

import Prelude ((<>),Unit,pure,unit,discard,($),show)
import Effect (Effect)
import Data.Nullable (Nullable,toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List.NonEmpty (head)

import Signal (Signal)
import WebAudio (WebAudioContext,WebAudioNode)
import W

type AudioWorklet = {
  name :: String,
  code :: String,
  connected :: Boolean,
  audioWorkletNode :: Nullable WebAudioNode
  }

runWorklet :: WebAudioContext -> WebAudioNode -> String -> Signal -> Number -> Number -> Effect AudioWorklet
runWorklet ctx dest name s fInStart fInDur = _runWorklet ctx dest name (code s name fInStart fInDur)

foreign import _runWorklet :: WebAudioContext -> WebAudioNode -> String -> String -> Effect AudioWorklet

stopWorklet :: AudioWorklet -> Number -> Number -> Effect Unit
stopWorklet w fOutStart fOutDur = do
  case toMaybe w.audioWorkletNode of
    Nothing -> pure unit
    Just n -> do
      setWorkletParamValue n "fOutStart" fOutStart
      setWorkletParamValue n "fOutDur" fOutDur
      -- not sure if we also need to have a scheduled disconnect of the processor?

foreign import setWorkletParamValue :: WebAudioNode -> String -> Number -> Effect Unit

code :: Signal -> String -> Number -> Number -> String
code s name fInStart fInDur = prefix <> classHeader <> getParameterDescriptors <> constructor <> innerLoopPrefix <> innerLoop <> fadeInCalculation <> restOfClass <> registerProcessor
  where
    Tuple frame wState = runW $ signalToFrame s
    prefix = "function clamp(min,max,x) { return Math.max(Math.min(max,x),min); }\n\n"
    classHeader = "class " <> name <> " extends AudioWorkletProcessor {\n\n"
    getParameterDescriptors = "static get parameterDescriptors() { return [{ name:'fOutStart', defaultValue:-1.0 },{ name:'fOutDur', defaultValue:5.0 }]; }\n\n"
    constructor = "constructor() { super(); this.framesOut = 0; this.runTime = currentTime; this.m = new Float32Array(" <> show wState.allocation <> ")}\n\n"
    innerLoopPrefix = """process(inputs,outputs,parameters) {
const input = inputs[0];
const output = outputs[0];
const blockSize = 128;
const fOutDur = parameters.fOutDur[0];
const fOutEnd = parameters.fOutStart[0] == -1.0 ? -1.0 : parameters.fOutStart[0] + fOutDur;
const m = this.m;
for(let n=0; n<blockSize; n++){
const t = currentTime + (n/sampleRate);
"""
    innerLoop = wState.code <> "output[0][n] = " <> showSample (head frame) <> ";\n"
    fadeInCalculation = "const fIn = clamp(0,1,(t-" <> show fInStart <> ")/" <> show fInDur <> ");"
    restOfClass = """
const fOut = fOutEnd == -1.0 ? 1.0 : clamp(0,1,(fOutEnd-t)/fOutDur);
const f = Math.min(fIn,fOut);
output[0][n] = output[0][n] * f;
}
this.framesOut += blockSize;
return (fOutEnd == -1.0 ? true : (currentTime + (blockSize/sampleRate) <= fOutEnd));
}
}

"""
    registerProcessor = "registerProcessor('" <> name <> "'," <> name <> ");\n"


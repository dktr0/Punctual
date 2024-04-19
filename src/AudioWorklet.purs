module AudioWorklet where

import Prelude ((<>),Unit,pure,unit,discard,($),show,(>>=),(-))
import Effect (Effect)
import Data.Nullable (Nullable,toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List.NonEmpty (length,zipWith)
import Data.Unfoldable1 (range)
import Data.Foldable (fold)

import Signal (Signal)
import WebAudio (WebAudioContext,WebAudioNode)
import W
import AudioPanning (splay)
import Multi (flatten)

type AudioWorklet = {
  name :: String,
  code :: String,
  connected :: Boolean,
  audioWorkletNode :: Nullable WebAudioNode
  }

runWorklet :: WebAudioContext -> WebAudioNode -> String -> Signal -> Number -> Number -> Effect AudioWorklet
runWorklet ctx dest name s fInStart fInDur = _runWorklet ctx dest name (code s name fInStart fInDur) 2

foreign import _runWorklet :: WebAudioContext -> WebAudioNode -> String -> String -> Int -> Effect AudioWorklet

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
code s name fInStart fInDur = prefix <> classHeader <> getParameterDescriptors <> constructor <> innerLoopPrefix <> fadeCalculations <> wState.code <> outputs <> debug <> restOfClass <> registerProcessor
  where
    Tuple frameMulti wState = runW $ signalToFrame s >>= splay 2
    frame = flatten frameMulti
    prefix = """'use strict';

function clamp(min,max,x) { return Math.max(Math.min(max,x),min); }

"""
    classHeader = "class " <> name <> " extends AudioWorkletProcessor {\n\n"
    getParameterDescriptors = """static get parameterDescriptors() {
return [
  { name:'fOutStart', defaultValue:-1.0 },
  { name:'fOutDur', defaultValue:5.0 },
  { name:'cps', defaultValue:1.0 },
  { name:'origin', defaultValue:0.0 },
  { name:'evalTimeAudio',defaultValue:0.0 }
];}

"""
    constructor = "constructor() { super(); this.framesOut = 0; this.runTime = currentTime; this.m = new Float32Array(" <> show wState.allocation <> ")}\n\n"
    innerLoopPrefix = """process(inputs,outputs,parameters) {
const input = inputs[0];
const output = outputs[0];
const blockSize = 128;
const cps = parameters.cps[0];
const origin = parameters.origin[0];
const evalTimeAudio = parameters.evalTimeAudio[0];
const fOutDur = parameters.fOutDur[0];
const fOutEnd = parameters.fOutStart[0] == -1.0 ? -1.0 : parameters.fOutStart[0] + fOutDur;
const m = this.m;
for(let n=0; n<blockSize; n++){
const t = currentTime + (n/sampleRate);
const time = t - origin;
const beat = time * cps;
const eTime = 0.0; // t - evalTimeAudio;
const eBeat = eTime * cps;
const fOut = fOutEnd == -1.0 ? 1.0 : clamp(0,1,(fOutEnd-t)/fOutDur);
"""
    fadeCalculations = "const fIn = clamp(0,1,(t-" <> show fInStart <> ")/" <> show fInDur <> ");\nconst f = Math.min(fIn,fOut);\n"
    outputIndices = range 0 (length frame - 1)
    outputF i x = "output[" <> show i <> "][n] = " <> showSample x <> "*f*0.1;\n"
    outputs = fold $ zipWith outputF outputIndices frame
    debug = ("// signal:" <> show s <> "\n") <> ("// frameMulti:" <> show frameMulti <> "\n") <> ("// frame:" <> show frame <> "\n")
    restOfClass = """}
this.framesOut += blockSize;
return (fOutEnd == -1.0 ? true : (currentTime + (blockSize/sampleRate) <= fOutEnd));
}
}

"""
    registerProcessor = "registerProcessor('" <> name <> "'," <> name <> ");\n"


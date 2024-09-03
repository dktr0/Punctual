module AudioWorklet where

import Prelude ((<>),Unit,pure,unit,discard,($),show,(>>=),(-),bind)
import Effect (Effect)
import Data.Nullable (Nullable,toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List.NonEmpty (length,zipWith)
import Data.Unfoldable1 (range)
import Data.Foldable (fold)
import Effect.Class.Console (log)

import Signal (Signal)
import WebAudio (WebAudioContext,WebAudioNode)
import W
import AudioPanning (splay)
import Matrix (flatten)

type AudioWorklet = {
  name :: String,
  signal :: Signal,
  code :: String,
  audioWorklet' :: AudioWorklet'
  }
  
type AudioWorklet' = {
  connected :: Boolean,
  audioWorkletNode :: Nullable WebAudioNode
  }

runWorklet :: WebAudioContext -> Nullable WebAudioNode -> WebAudioNode -> String -> Signal -> Number -> Number -> Effect AudioWorklet
runWorklet ctx ain aout name signal fInStart fInDur = do
  let code = generateWorkletCode signal name fInStart fInDur
  log code
  audioWorklet' <- _runWorklet ctx ain aout name code 2
  pure { name, signal, code, audioWorklet' }

foreign import _runWorklet :: WebAudioContext -> Nullable WebAudioNode -> WebAudioNode -> String -> String -> Int -> Effect AudioWorklet'

stopWorklet :: AudioWorklet -> Number -> Number -> Effect Unit
stopWorklet w fOutStart fOutDur = do
  case toMaybe w.audioWorklet'.audioWorkletNode of
    Nothing -> pure unit
    Just n -> do
      setWorkletParamValue n "fOutStart" fOutStart
      setWorkletParamValue n "fOutDur" fOutDur
      -- not sure if we also need to have a scheduled disconnect of the processor?

foreign import setWorkletParamValue :: WebAudioNode -> String -> Number -> Effect Unit


generateWorkletCode :: Signal -> String -> Number -> Number -> String
generateWorkletCode s name fInStart fInDur = prefix <> classHeader <> getParameterDescriptors <> constructor <> innerLoopPrefix <> fadeCalculations <> wState.code <> outputs <> debug <> restOfClass <> registerProcessor
  where
    Tuple frameMulti wState = runW $ signalToFrame s >>= splay 2
    frame = flatten frameMulti
    prefix = """'use strict';

function clamp(min,max,x) { return Math.max(Math.min(max,x),min); }
function ain(input,n) { return (n >= input.length ? 0.0 : input[n]); }

"""
    classHeader = "class " <> name <> " extends AudioWorkletProcessor {\n\n"
    getParameterDescriptors = """static get parameterDescriptors() {
return [
  { name:'fOutStart', defaultValue:-1.0 },
  { name:'fOutDur', defaultValue:5.0 },
  { name:'cps', defaultValue:1.0 },
  { name:'originAudio', defaultValue:0.0 },
  { name:'evalTimeAudio', defaultValue:0.0 }
];}

"""
    constructor = "constructor() { super(); this.framesOut = 0; this.runTime = currentTime; this.f = new Float32Array(" <> show wState.allocatedFloats <> ").fill(0); this.i = new Int32Array(" <> show wState.allocatedInts <> ").fill(0);}\n\n"
    innerLoopPrefix = """process(inputs,outputs,parameters) {
const input = inputs[0];
const output = outputs[0];
const blockSize = 128;
const cps = parameters.cps[0];
const originAudio = parameters.originAudio[0];
const evalTimeAudio = parameters.evalTimeAudio[0];
const fOutDur = parameters.fOutDur[0];
const fOutEnd = parameters.fOutStart[0] == -1.0 ? -1.0 : parameters.fOutStart[0] + fOutDur;
const f = this.f;
const i = this.i;
for(let n=0; n<blockSize; n++){
const t = currentTime + (n/sampleRate);
const time = t - originAudio;
const beat = time * cps;
const eTime = t - evalTimeAudio;
const eBeat = eTime * cps;
const fOut = fOutEnd == -1.0 ? 1.0 : clamp(0,1,(fOutEnd-t)/fOutDur);
"""
    fadeCalculations = "const fIn = clamp(0,1,(t-" <> show fInStart <> ")/" <> show fInDur <> ");\nconst fade = Math.min(fIn,fOut);\n"
    outputIndices = range 0 (length frame - 1)
    outputF i x = "output[" <> show i <> "][n] = " <> showSample x <> "*fade*0.1;\n"
    outputs = fold $ zipWith outputF outputIndices frame
    debug = ("// signal:" <> show s <> "\n") <> ("// frameMulti:" <> show frameMulti <> "\n") <> ("// frame:" <> show frame <> "\n")
    restOfClass = """}
this.framesOut += blockSize;
return (fOutEnd == -1.0 ? true : (currentTime + (blockSize/sampleRate) <= fOutEnd));
}
}

"""
    registerProcessor = "registerProcessor('" <> name <> "'," <> name <> ");\n"


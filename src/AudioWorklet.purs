module AudioWorklet where

import Prelude ((<>),Unit,pure,discard,($),show,(>>=),(-),bind,(+))
import Effect (Effect)
import Data.Nullable (Nullable,toMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List.NonEmpty (zipWith)
import Data.Unfoldable1 (range)
import Data.Foldable (fold)
import Effect.Class.Console (log)

import Signal (Signal)
import WebAudio (WebAudioContext,WebAudioNode)
import W
import Output (Output,audioOutputChannels,audioOutputOffset)

type AudioWorklet = {
  name :: String,
  signal :: Signal,
  output :: Output,
  code :: String,
  audioWorklet' :: AudioWorklet'
  }
  
type AudioWorklet' = {
  connected :: Boolean,
  audioWorkletNode :: Nullable WebAudioNode
  }

runWorklet :: WebAudioContext -> Nullable WebAudioNode -> WebAudioNode -> String -> Signal -> Int -> Output -> Number -> Number -> Effect AudioWorklet
runWorklet ctx ain aout name signal maxChnls output fInStart fInDur = do
  let nOutputChnls = audioOutputChannels maxChnls output 
  let channelOffset = audioOutputOffset output
  let code = generateWorkletCode signal nOutputChnls channelOffset name fInStart fInDur
  audioWorklet' <- _runWorklet ctx ain aout name code (nOutputChnls+channelOffset)
  pure { name, signal, output, code, audioWorklet' }

foreign import _runWorklet :: WebAudioContext -> Nullable WebAudioNode -> WebAudioNode -> String -> String -> Int -> Effect AudioWorklet'

stopWorklet :: AudioWorklet -> Number -> Number -> Effect Unit
stopWorklet w fOutStart fOutDur = do
  case toMaybe w.audioWorklet'.audioWorkletNode of
    Nothing -> log "strange internal error in Punctual: attempt to stop AudioWorklet with no valid WebAudioNode"
    Just n -> do
      setWorkletParamValue n "fOutStart" fOutStart
      setWorkletParamValue n "fOutDur" fOutDur
      -- not sure if we also need to have a scheduled disconnect of the processor?

foreign import setWorkletParamValue :: WebAudioNode -> String -> Number -> Effect Unit

updateWorklet :: Number -> Number -> Number -> AudioWorklet -> Effect Unit
updateWorklet cps originAudio evalTimeAudio w = do
  case toMaybe w.audioWorklet'.audioWorkletNode of
    Nothing -> log "strange internal error in Punctual: attempt to update tempo for AudioWorklet with no valid WebAudioNode"
    Just n -> do
      setWorkletParamValue n "cps" cps
      setWorkletParamValue n "originAudio" originAudio
      setWorkletParamValue n "evalTimeAudio" evalTimeAudio

generateWorkletCode :: Signal -> Int -> Int -> String -> Number -> Number -> String
generateWorkletCode s nOutputChnls channelOffset name fInStart fInDur = prefix <> classHeader <> getParameterDescriptors <> constructor <> innerLoopPrefix <> fadeCalculations <> wState.code <> outputs <> restOfClass <> registerProcessor
  where
    Tuple frame wState = runW $ signalToFrame s >>= aout nOutputChnls channelOffset
    prefix = """'use strict';

function clamp(min,max,x) { return Math.max(Math.min(max,x),min); }
function ain(input,n) { return (n >= input.length ? 0.0 : input[n]); }
function genSin() {
  var r = new Float32Array(16384).fill(0);
  for(var t=0;t<16384;t++) {
    r[t] = Math.sin(Math.PI * 2.0 * t / 16384);
  }
  return r;
}
function genSaw() {
  var r = new Float32Array(4096).fill(0.5);
  for(var k=1;k<=84;k++) { // with 84 harmonics, highest harmonic of middle C is just below 22050 Hz
    var x = Math.pow(-1,k);
    for(var t=0;t<4096;t++) {
      r[t] -= Math.sin(Math.PI * 2.0 * k * t / 4096) * x / (k * Math.PI);
    }
  }
  return r;
}
function genSqr() {
  var r = new Float32Array(4096).fill(0.5);
  for(var k=1;k<=83;k+=2) { // with 84 harmonics, highest harmonic of middle C is just below 22050 Hz
    for(var t=0;t<4096;t++) {
      r[t] += Math.sin(Math.PI * 2.0 * k * t / 4096) * 4 / (k * Math.PI);
    }
  }
  return r;
}
function genTri() {
  var r = new Float32Array(4096).fill(0.5);
  for(var k=1;k<=83;k+=2) { // with 84 harmonics, highest harmonic of middle C is just below 22050 Hz
    var x = Math.pow(-1,(k-1)*0.5);
    for(var t=0;t<4096;t++) {
      r[t] += Math.sin(Math.PI * 2.0 * k * t / 4096) * 8 * x / (k * k * Math.PI * Math.PI);
    }
  }
  return r;
}

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
    constructor = "constructor() { super(); this.sin=genSin(); this.saw=genSaw(); this.sqr=genSqr(); this.tri=genTri(); this.framesOut=0; this.runTime=currentTime; this.f=new Float32Array(" <> show wState.allocatedFloats <> ").fill(0); this.i=new Int32Array(" <> show wState.allocatedInts <> ").fill(0);}\n\n"
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
const sin = this.sin;
const saw = this.saw;
const sqr = this.sqr;
const tri = this.tri;
"""
    fadeCalculations = "const fIn = clamp(0,1,(t-" <> show fInStart <> ")/" <> show fInDur <> ");\nconst fade = Math.min(fIn,fOut);\n"
    outputIndices = range 0 (nOutputChnls + channelOffset - 1)
    outputF i x = "if(output[" <> show i <> "]!=null){output[" <> show i <> "][n] = " <> showSample x <> "*fade};\n"
    outputs = fold $ zipWith outputF outputIndices frame
    restOfClass = """}
this.framesOut += blockSize;
return (fOutEnd == -1.0 ? true : (currentTime + (blockSize/sampleRate) <= fOutEnd));
}
}

"""
    registerProcessor = "registerProcessor('" <> name <> "'," <> name <> ");\n"

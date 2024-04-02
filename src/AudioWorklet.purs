module AudioWorklet where

import Prelude ((<>),show,Unit,pure,unit,bind,discard,(+))
import Effect (Effect)
import Data.Nullable (Nullable,toMaybe)
import Data.Maybe (Maybe(..))

import Signal
import WebAudio (WebAudioContext,WebAudioNode,currentTime)


type AudioWorklet = {
  name :: String,
  code :: String,
  connected :: Boolean,
  audioWorkletNode :: Nullable WebAudioNode
  }

runWorklet :: WebAudioContext -> WebAudioNode -> String -> Signal -> Effect AudioWorklet
runWorklet ctx dest name s = _runWorklet ctx dest name (code s name)

foreign import _runWorklet :: WebAudioContext -> WebAudioNode -> String -> String -> Effect AudioWorklet

stopWorklet :: WebAudioContext -> AudioWorklet -> Effect Unit
stopWorklet ctx w = do
  case toMaybe w.audioWorkletNode of
    Nothing -> pure unit
    Just n -> do
      t <- currentTime ctx
      setWorkletParamValue n "fOutStart" (t + 0.5)
      setWorkletParamValue n "fOutDur" 5.0
      -- not sure if we also need to have a scheduled disconnect of the processor?

foreign import setWorkletParamValue :: WebAudioNode -> String -> Number -> Effect Unit

code :: Signal -> String -> String
code s name = part1 <> part2 <> innerLoop s <> part4 <> part5
  where
    part1 = "class " <> name <> " extends AudioWorkletProcessor {\n"
    part2 = """static get parameterDescriptors() { return [
{ name: 'fInStart', defaultValue: 0.0 },
{ name: 'fInDur', defaultValue: 5.0 },
{ name: 'fOutStart', defaultValue: -1.0 },
{ name: 'fOutDur', defaultValue: 5.0 }
];}
constructor() { super(); this.framesOut = 0; this.runTime = currentTime }
clamp(min,max,x) { return Math.max(Math.min(max,x),min); }
process(inputs,outputs,parameters) {
const input = inputs[0];
const output = outputs[0];
const fInStart = parameters.fInStart[0] == 0.0 ? this.runTime : parameters.fInStart[0];
const fInDur = parameters.fInDur[0];
const fOutStart = parameters.fOutStart[0] == -1.0 ? -1.0 : parameters.fOutStart[0];
const fOutDur = parameters.fOutDur[0];
const blockSize = 128;
for(let n=0; n<blockSize; n++){
const t = currentTime + (n/sampleRate);
"""
    part4 = """
const fIn = this.clamp(0,1, (t-fInStart)/fInDur);
const fOut = fOutStart == -1.0 ? 1.0 : this.clamp(0,1,(fOutStart+fOutDur-t)/fOutDur);
const f = Math.min(fIn,fOut);
output[0][n] = output[0][n] * f;
}
this.framesOut += blockSize;
return (fOutStart == -1.0 ? true : (currentTime + (blockSize/sampleRate) <= fOutStart + fOutDur));
}
}
"""
    part5 = "registerProcessor('" <> name <> "'," <> name <> ");\n"

innerLoop :: Signal -> String
innerLoop (Osc (Constant f)) = "output[0][n] = Math.sin(t * 2.0 * Math.PI * " <> show f <> ") * 0.1;\n"
-- "output[0][n] = Math.sin( (currentTime - this.runTime + (n/sampleRate)) * 2.0 * Math.PI * " <> show f <> ");\n"
-- "output[0][n] = Math.sin( (this.framesOut + n) * 2.0 * Math.PI * " <> show f <> " / sampleRate);\n"
innerLoop Rnd = "output[0][n] = Math.random();\n"
innerLoop _ = "output[0][n] = 0;\n"


module AudioWorklet where

import Prelude ((<>),show)
import Effect (Effect)
import Data.Nullable (Nullable)

import Signal
import WebAudio (WebAudioContext,WebAudioNode)


type AudioWorklet = {
  name :: String,
  code :: String,
  connected :: Boolean,
  audioWorkletNode :: Nullable WebAudioNode
  }

runWorklet :: WebAudioContext -> WebAudioNode -> String -> Signal -> Effect AudioWorklet
runWorklet ctx dest name s = _runWorklet ctx dest name (code s name)
  
foreign import _runWorklet :: WebAudioContext -> WebAudioNode -> String -> String -> Effect AudioWorklet

code :: Signal -> String -> String
code s name = part1 <> part2 <> innerLoop s <> part4 <> part5
  where
    part1 = "class " <> name <> " extends AudioWorkletProcessor {\n"
    part2 = """constructor() { super(); }
process(inputs,outputs,parameters) {
const input = inputs[0];
const output = outputs[0];
const blockSize = 128;
for(let n=0; n<blockSize; n++){
"""
    part4 = """}
return true;
}
}
"""
    part5 = "registerProcessor('" <> name <> "'," <> name <> ");\n"

innerLoop :: Signal -> String
innerLoop (Osc f) = "output[0][n] = Math.sin(Math.PI * n * " <> show f <> ");\n" -- placeholder
innerLoop _ = "output[0][n] = 0;\n"





"use strict";

export const _runWorklet = ctx => ain => aout => name => code => nOutputChnls => () => {
  var r = { connected: false, audioWorkletNode: null };
  var url = window.URL.createObjectURL( new Blob( [code], { type: 'text/javascript' }));
  ctx.audioWorklet.addModule(url).then( () => {
    const node = new AudioWorkletNode(ctx,name,{outputChannelCount:[nOutputChnls],channelInterpretation:"discrete"});
    if(ain != null) { ain.connect(node); }
    node.connect(aout);
    r.audioWorkletNode = node;
    r.connected = true;
    });
  return r;
  }
  
export const setWorkletParamValue = n => k => v => () => { n.parameters.get(k).value = v; }



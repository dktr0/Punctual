"use strict";

export const _runWorklet = ctx => dest => name => code => () => {
  var r = { name: name, code: code, connected: false, audioWorkletNode: null };
  var url = window.URL.createObjectURL( new Blob( [code], { type: 'text/javascript' }));
  ctx.audioWorklet.addModule(r.url).then( () => 
    var node = new AudioWorkletNode(ctx,name);
    node.connect(dest);
    r.audioWorkletNode = node;
    r.connected = true;
    );
  return r;
  }


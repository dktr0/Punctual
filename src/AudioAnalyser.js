"use strict";

export const defaultWebAudioContext = () => new AudioContext();

export const resumeWebAudioContext = ac => () => ac.resume();

export const gainNode = ac => gain => () => new GainNode(ac,{ gain:gain });

export const _analyserNode = ac => fftSize => smoothingTimeConstant => () => new AnalyserNode(ac,{ fftSize:fftSize, smoothingTimeConstant:smoothingTimeConstant });

export const _defaultAudioInputNode = ac => () => {
  var r = new GainNode(ac);
  navigator.mediaDevices.getUserMedia({ audio: true, video: false}).then(function(stream) {
    var x = new MediaStreamAudioSourceNode(ac,{mediaStream: stream});
    x.connect(r);
  });
  return r;
  }

export const _connect = src => dest => () => src.connect(dest);

export const _disconnect = src => dest => () => src.disconnect(dest);

export const _analyserArray = binCount => () => new Uint8Array(binCount);

export const _getByteFrequencyData = node => array => () => node.getByteFrequencyData(array);

export const _getLo = array => () => { var acc=0; for(var x=0;x<8;x++) { acc=acc+array[x] }; acc=acc/(8*256); return acc; }
export const _getMid = array => () => { var acc=0; for(var x=0;x<80;x++) { acc=acc+array[x] }; acc=acc/(72*256); return acc; }
export const _getHi = array => () => { var acc=0; for(var x=0;x<512;x++) { acc=acc+array[x] }; acc=acc/(432*256); return acc; }


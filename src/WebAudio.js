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


"use strict";

export const _newWebcamElement = () => {
  var r = document.createElement('video');
  r.width = 2048; r.height = 2048; r.autoplay = true; r.isPlaying = false;
  r.addEventListener('playing',function() { r.isPlaying = true; });
  navigator.mediaDevices.getUserMedia({video: true}).then( function(stream) { r.srcObject = stream; } );
  return r;
  }
  
export const _stopWebcamElement = e => () => e.srcObject.getTracks().forEach(function(track) { track.stop(); });

export const _updateWebcamTexture = gl => t => e => () => {
  if(e.isPlaying) {
    gl.activeTexture(gl.TEXTURE3);
    gl.bindTexture(gl.TEXTURE_2D,t);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, e);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    }
  }


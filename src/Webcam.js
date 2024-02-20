"use strict";

export const newWebcamElement = () => {
  var r = document.createElement('video');
  r.width = 2048; r.height = 2048; r.autoplay = true; r.isPlaying = false;
  r.addEventListener('playing',function() { r.isPlaying = true; });
  navigator.mediaDevices.getUserMedia({video: true}).then( function(stream) { r.srcObject = stream; } );
  return r;
  }
  
export const stopWebcamElement = e => () => e.srcObject.getTracks().forEach(function(track) { track.stop(); });

export const updateWebcamTexture = glc => t => e => () => {
  if(e.isPlaying) {
    glc.gl.activeTexture(glc.gl.TEXTURE3);
    glc.gl.bindTexture(glc.gl.TEXTURE_2D,t);
    glc.gl.texImage2D(glc.gl.TEXTURE_2D, 0, glc.gl.RGBA, glc.gl.RGBA, glc.gl.UNSIGNED_BYTE, e);
    glc.gl.texParameteri(glc.gl.TEXTURE_2D, glc.gl.TEXTURE_WRAP_S, glc.gl.CLAMP_TO_EDGE);
    glc.gl.texParameteri(glc.gl.TEXTURE_2D, glc.gl.TEXTURE_WRAP_T, glc.gl.CLAMP_TO_EDGE);
    glc.gl.texParameteri(glc.gl.TEXTURE_2D, glc.gl.TEXTURE_MIN_FILTER, glc.gl.LINEAR);
    }
  }


"use strict";

export const createCanvas = () => {
  var r = document.createElement('canvas');
  r.setAttribute("width","1920");
  r.setAttribute("height","1080");
  r.setAttribute("style","z-index: -1; position: absolute; width: 100%; height: 100%; left: 0px; top:0px; pointer-events: none");
  return r;
  }

export const appendCanvasToDocumentBody = canvas => () => document.body.appendChild(canvas);

export const deleteCanvasFromDocumentBody = canvas => () => document.body.removeChild(canvas);

export const _getWebGL1Context = canvas => () => canvas.getContext('webgl',{ powerPreference: 'high-performance', antialias: true });

export const _getWebGL2Context = canvas => () => canvas.getContext('webgl2',{ powerPreference: 'high-performance', antialias: true });

export const _getExtension = context => name => () => context.getExtension(name);


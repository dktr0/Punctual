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

export const defaultBlendFunc = gl => () => { gl.enable(gl.BLEND); gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA); }

export const unpackFlipY = gl => () => gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);

export const createBuffer = gl => () => gl.createBuffer();

export const bindBufferArray = gl => buffer => () => gl.bindBuffer(gl.ARRAY_BUFFER,buffer);

export const bufferData_defaultTriangleStrip = gl => () => gl.bufferData(gl.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),gl.STATIC_DRAW);

export const createProgram = gl => () => gl.createProgram();

export const createVertexShader = gl => () => gl.createShader(gl.VERTEX_SHADER);

export const createFragmentShader = gl => () => gl.createShader(gl.FRAGMENT_SHADER);

export const attachShader = gl => p => s => () => gl.attachShader(p,s);

export const shaderSource = gl => s => txt => () => gl.shaderSource(s,txt);

export const compileShader = gl => s => () => gl.compileShader(s);

export const linkProgram = gl => p => () => gl.linkProgram(p);

export const flush = gl => () => gl.flush();

export const useProgram = gl => p => () => gl.useProgram(p);

export const getAttribLocation = gl => p => n => () => gl.getAttribLocation(p,n);

export const vertexAttribPointer = gl => loc => () => gl.vertexAttribPointer(loc,2,gl.FLOAT,false,0,0);

export const enableVertexAttribArray = gl => loc => () => gl.enableVertexAttribArray(loc);

export const viewport = gl => x => y => w => h => () => gl.viewport(x,y,w,h);

export const clearColor = gl => r => g => b => a => () => gl.clearColor(r,g,b,a);

export const clearColorBuffer = gl => () => gl.clear(gl.COLOR_BUFFER_BIT);

export const drawDefaultTriangleStrip = gl => () => gl.drawArrays(gl.TRIANGLE_STRIP,0,4);

export const getUniformLocation = gl => p => n => () => gl.getUniformLocation(p,n);

export const uniform1f = gl => loc => x => () => gl.uniform1f(loc,x);

export const uniform2f = gl => loc => x => y => () => gl.uniform2f(loc,x,y);


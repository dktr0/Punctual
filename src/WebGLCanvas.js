"use strict";

export const createHTMLCanvasElement = () => {
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

export const _getExtension = gl => name => () => gl.getExtension(name);

export const defaultBlendFunc = gl => () => { gl.enable(gl.BLEND); gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA); }

export const unpackFlipY = gl => () => gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);

export const createBuffer = glc => () => glc.gl.createBuffer();

export const bindBufferArray = glc => buffer => () => glc.gl.bindBuffer(glc.gl.ARRAY_BUFFER,buffer);

export const bufferData_defaultTriangleStrip = glc => () => glc.gl.bufferData(glc.gl.ARRAY_BUFFER,new Float32Array([-1,1,-1,-1,1,1,1,-1]),glc.gl.STATIC_DRAW);

export const createProgram = glc => () => glc.gl.createProgram();

export const createVertexShader = glc => () => glc.gl.createShader(glc.gl.VERTEX_SHADER);

export const createFragmentShader = glc => () => glc.gl.createShader(glc.gl.FRAGMENT_SHADER);

export const attachShader = glc => p => s => () => glc.gl.attachShader(p,s);

export const shaderSource = glc => s => txt => () => glc.gl.shaderSource(s,txt);

export const compileShader = glc => s => () => glc.gl.compileShader(s);

export const linkProgram = glc => p => () => glc.gl.linkProgram(p);

export const flush = glc => () => glc.gl.flush();

export const useProgram = glc => p => () => glc.gl.useProgram(p);

export const getAttribLocation = glc => p => n => () => glc.gl.getAttribLocation(p,n);

export const vertexAttribPointer = glc => loc => () => glc.gl.vertexAttribPointer(loc,2,glc.gl.FLOAT,false,0,0);

export const enableVertexAttribArray = glc => loc => () => glc.gl.enableVertexAttribArray(loc);

export const viewport = glc => x => y => w => h => () => glc.gl.viewport(x,y,w,h);

export const clearColor = glc => r => g => b => a => () => glc.gl.clearColor(r,g,b,a);

export const clearColorBuffer = glc => () => glc.gl.clear(glc.gl.COLOR_BUFFER_BIT);

export const drawDefaultTriangleStrip = glc => () => glc.gl.drawArrays(glc.gl.TRIANGLE_STRIP,0,4);

export const getUniformLocation = glc => p => n => () => glc.gl.getUniformLocation(p,n);

export const _uniform1i = gl => loc => x => () => gl.uniform1i(loc,x);

export const _uniform1f = gl => loc => x => () => gl.uniform1f(loc,x);

export const _uniform2f = gl => loc => x => y => () => gl.uniform2f(loc,x,y);

export const _createTexture = gl => () => gl.createTexture();

export const _activeTexture0 = gl => () => gl.activeTexture(gl.TEXTURE0);
export const _activeTexture1 = gl => () => gl.activeTexture(gl.TEXTURE1);
export const _activeTexture2 = gl => () => gl.activeTexture(gl.TEXTURE2);
export const _activeTexture3 = gl => () => gl.activeTexture(gl.TEXTURE3);
export const _activeTexture4 = gl => () => gl.activeTexture(gl.TEXTURE4);
export const _activeTexture5 = gl => () => gl.activeTexture(gl.TEXTURE5);
export const _activeTexture6 = gl => () => gl.activeTexture(gl.TEXTURE6);
export const _activeTexture7 = gl => () => gl.activeTexture(gl.TEXTURE7);
export const _activeTexture8 = gl => () => gl.activeTexture(gl.TEXTURE8);
export const _activeTexture9 = gl => () => gl.activeTexture(gl.TEXTURE9);
export const _activeTexture10 = gl => () => gl.activeTexture(gl.TEXTURE10);
export const _activeTexture11 = gl => () => gl.activeTexture(gl.TEXTURE11);
export const _activeTexture12 = gl => () => gl.activeTexture(gl.TEXTURE12);
export const _activeTexture13 = gl => () => gl.activeTexture(gl.TEXTURE13);
export const _activeTexture14 = gl => () => gl.activeTexture(gl.TEXTURE14);
export const _activeTexture15 = gl => () => gl.activeTexture(gl.TEXTURE15);

export const _bindTexture2D = gl => t => () => gl.bindTexture(gl.TEXTURE_2D,t);


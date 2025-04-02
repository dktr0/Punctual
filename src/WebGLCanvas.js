"use strict";

export const createHTMLCanvasElement = () => {
  var r = document.createElement('canvas');
//  r.setAttribute("width","1000");
//  r.setAttribute("height","1000");
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

export const _createProgram = gl => () => gl.createProgram();

export const _createVertexShader = gl => () => gl.createShader(gl.VERTEX_SHADER);

export const _createFragmentShader = gl => () => gl.createShader(gl.FRAGMENT_SHADER);

export const _attachShader = gl => p => s => () => gl.attachShader(p,s);

export const _shaderSource = gl => s => txt => () => gl.shaderSource(s,txt);

export const _compileShader = gl => s => () => gl.compileShader(s);

export const _linkProgram = gl => p => () => gl.linkProgram(p);

export const _flush = gl => () => gl.flush();

export const _getShaderParameterCompileStatus = gl => s => () => gl.getShaderParameter(s,gl.COMPILE_STATUS);

export const _getShaderInfoLog = gl => s => () => gl.getShaderInfoLog(s);

export const _getProgramInfoLog = gl => p => () => gl.getProgramInfoLog(p); 

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

export const _createFrameBuffer = gl => () => gl.createFramebuffer();

export const _initializeFrameBufferTexture = gl => texture => frameBuffer => width => height => () => {
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.bindFramebuffer(gl.FRAMEBUFFER, frameBuffer);
  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  }

export const _reinitializeFrameBufferTexture = gl => texture => width => height => () => {
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  }
  
export const _bindFrameBuffer = gl => x => () => gl.bindFramebuffer(gl.FRAMEBUFFER,x);

export const _getCanvasWidth = c => () => Math.ceil(c.clientWidth * window.devicePixelRatio);

export const _getCanvasHeight = c => () => Math.ceil(c.clientHeight * window.devicePixelRatio);

export const getDevicePixelRatio = () => window.devicePixelRatio;

export const _harmonizeCanvasDimensions = c => () => {
  c.width = c.clientWidth * window.devicePixelRatio;
  c.height = c.clientHeight * window.devicePixelRatio;
  }


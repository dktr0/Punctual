import * as P from './output/Main/index.js';

export function exoLang() {
  return new Punctual();
}

export function Punctual() {
  this.punctual = P.launch();
}

Punctual.prototype.define = function(args) {
  return P.define(this.punctual)(args)();
}

Punctual.prototype.clear = function(args) {
  return P.clear(this.punctual)(args)();
}

Punctual.prototype.preRender = function(args) {
  return P.preRender(this.punctual)(args)();
}

Punctual.prototype.render = function(args) {
  return P.render(this.punctual)(args)();
}

Punctual.prototype.postRender = function(args) {
  return P.postRender(this.punctual)(args)();
}

Punctual.prototype.setTempo = function(foreignTempo) {
  return P.setTempo(this.punctual)(foreignTempo)();
}

Punctual.prototype.setAudioInput = function(effectWebAudioNode) {
  return P.setAudioInput(this.punctual)(effectWebAudioNode)();
}

Punctual.prototype.setAudioOutput = function(webAudioNode) {
  return P.setAudioOutput(this.punctual)(webAudioNode)();
}

Punctual.prototype.setBrightness = function(b) {
  return P.setBrightness(this.punctual)(b)();
}

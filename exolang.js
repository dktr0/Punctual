import * as P from './output/Main/index.js';

export function exoLang(canvas) {
  return new LocoMotion(canvas);
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

Punctual.prototype.setTempo = function(foreignTempo) {
  return P.setTempo(this.punctual)(foreignTempo)();
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


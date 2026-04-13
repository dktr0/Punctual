import * as P from './output/Main/index.js';

export function exoLang(args) {
  return new Punctual(args);
}

export function Punctual(args) {
  if (args==null) args = {};
  this.punctual = P.launch(args)();
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

Punctual.prototype.setOutputChannelCount = function(n) {
  console.log("punctual setOutputChannelCount " + n);
  return P.setOutputChannelCount(this.punctual)(n)();
}

Punctual.prototype.startAnimation = function() {
    this.timeOfStartAnimation = Date.now()/1000.0;
    this.framesSinceStartAnimation = 0;
    var p = this;
    window.requestAnimationFrame(function(){p.animate();});
}

Punctual.prototype.animate = function() {
    var p = this;
    window.requestAnimationFrame(function(){p.animate();});
    var now = Date.now()/1000.0;
    this.preRender({canDraw: true, nowTime: now});
    this.render({canDraw: true, zone:0, nowTime: now});
    this.postRender({canDraw: true, nowTime: now});
    this.framesSinceStartAnimation += 1;
}

Punctual.prototype.setFillModeScreen = function() {
  return P.setFillModeScreen(this.punctual)();
}

Punctual.prototype.setFillModePage = function() {
  return P.setFillModePage(this.punctual)();
}

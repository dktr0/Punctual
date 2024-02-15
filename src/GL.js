"use strict";

export const createCanvas = () => document.createElement('canvas');

export const _getWebGL1Context = canvas => () => canvas.getContext('webgl');

export const _getWebGL2Context = canvas => () => canvas.getContext('webgl2');


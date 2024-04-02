"use strict";

export const _analyserArray = binCount => () => new Uint8Array(binCount);

export const _getByteFrequencyData = node => array => () => node.getByteFrequencyData(array);

export const _getLo = array => () => { var acc=0; for(var x=0;x<8;x++) { acc=acc+array[x] }; acc=acc/(8*256); return acc; }
export const _getMid = array => () => { var acc=0; for(var x=0;x<80;x++) { acc=acc+array[x] }; acc=acc/(72*256); return acc; }
export const _getHi = array => () => { var acc=0; for(var x=0;x<512;x++) { acc=acc+array[x] }; acc=acc/(432*256); return acc; }


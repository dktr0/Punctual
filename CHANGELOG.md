# ChangeLog

0.5.0: 

Major changes relative to 0.4.x

-Punctual has been rewritten from the ground up in PureScript instead of Haskell, and is compiled as a JavaScript library (to be specific: an exolang for the Estuary platform)

-Libraries: addition of 'import' to include web-located libraries of Punctual code in programs (which also makes the evaluation of punctual code an asynchronous operation)

-Audio: all audio synthesis now happens in AudioWorklet nodes containing generated audio callback code (each distinct audio output statement in a punctual program is translated in to distinct AudioWorklet)

-Matrix semantics: previously the result of most punctual functions was essentially a non-empty *list* (a list of one or more items) of things that could be turned into time-varying signals (audio or light intensities). Now most of those same functions produce instead a non-empty two-dimensional matrix (rows and columns) of things that can be turned into time-varying signals, with rows typically representing combinatorial variations of an inner or lower level of information. Among other things, this allows for "multi-channel" use of seq.

-User-defined functions of any number of arguments, for example "f x y = osc [x,x+1,x*y,x*y+1];"

Other changes relative to 0.4.x

-[a,b,c] combines a b and c combinatorially (similar to 0.4 but with matrix semantics); {a,b,c}, which is new, combines a b and c pairwise; 'zip' is now deprecated (and will be removed in 0.6) since the new { } notation generalizes it

-available outputs are 'audio' 'blend' 'add' 'rgba' 'rgb' (previously existing outputs like 'hsv' 'red' 'splay' 'alpha' etc have been removed)

-'step' has been replaced with 'seq'

-introduction of 'slow' 'fast' 'early' and 'late' for time-shifting/stretching of arbitrary punctual expressions

-texture access functions(img vid cam ifft fft fb) don't take position arguments (if they did before), all instead stretch their results over the range of the unit square (in the case of fft and ifft, results are stretched over the x axis), and all are masked by the unit square (0 values outside of the coordinates lower than -1 or greater than 1)

-zoom now uses each channel of its first argument to scale both x and y dimensions; zoomxy, zoomx, and zoomy (new) can be used to zoom x and y axes independently

-tile now uses each channel of its first argument to modify both x and y dimensions; tilexy, tilex, and tiley (new) can be used to tile x and y axes independently

-multichannel audio: ain [nChnls] [offset], with mic as a synonym for 'ain 1 0' (the former audioin is a synonym for mic but is deprecated and will be removed in 0.6)

-the 'm' and 'db' syntaxes from early punctual have been removed (use the normal functions 'midicps' and 'dbamp' instead)

0.4.9.3:

-another bugfix affecting geometry transformations (spin, zoom, move, etc...)

0.4.9.2:

-bugfix affecting combinatorial geometry transformations (i.e. spin [x,y,z] $ tile [x,y] w)

0.4.9.1:

-bugfix to cbrt (visuals)

0.4.9:

-'add' as synonym for rgb output, 'blend' as synonym for rgba output (to ease forthcoming transition to new punctual 0.5 output modes)

0.4.8.1:

-bugfix affecting multiple layers of embedded arbitrary multichannel signals, eg. [[x,y],z]

0.4.8:

-added lines, linesp, ilines, ilinesp, chain, chainp, mesh and meshp. fixed further bugs affecting combinatorial geometrical transformations of shape functions

0.4.7.1:

-bugfix affecting combinatorial geometrical transformations of arbitrary multichannel signals (ie. [x,y,z])

0.4.7:

-added smoothstep/smoothstepp

0.4.4.9:

-fixed bugs affecting between, betweenp, clip, and clipp

0.4.4.8:

-added pairwise variants: circlep, rectp, vlinep, hlinep, linep, ilinep, clipp, betweenp, lpfp, hpfp, bpfp, ~~:, +=:, linlinp

0.4.4.7:

-added pairwise variants of max, min, and gate (maxp, minp, gatep)

0.4.4.6:

-fixed bug affecting multichannel use of trunc, acosh, asinh, atanh, cosh, log10, sinh, and tanh

0.4.4.5:

-fixed bug that was scrambling access to multiple image and video textures

0.4.4.4:

-bug fixes to precedence of pairwise arithmetic operators

0.4.4.3:

-bug fix (to [a,b .. c] expressions)

0.4.4.2:

-bug fix (xyt,xyrt,frt,ft)

0.4.4.1:

-bug fix (mod operators)

0.4.4:

-added polar coordinate queries and conversions: frt, fr, ft, xyrt, xyr, xyt, rtxy, rtx, rty 

0.4.3:

-added mod operators (% and %:), 'pi' and 20 other unary functions (basically completing coverage of JavaScript Math library's unary functions)
-added 'osc' as a synonym for the now deprecated 'sin'; as of version 0.5, 'osc' will be the oscillator, and 'sin' will be the sine function

0.4.2:

- webcam support officially added, via 'cam'
- bug fixes impacting audio input/output analysis 

0.4.1.2:

- bug fix impacting binary expressions (+ - etc) with multichannel geometry translations

0.4.1.1:

- bug fix impacting negative number literals and fragment shader code with GLSL operators

0.4.1:

- 'zip' added
- display of generated fragment shader code in standalone

0.4.0.3:

- bug fix to line and iline; anti-aliasing in line, iline, hline, vline, rect, circle, point

0.4.0.2:

- bug fix / change relating to clearing framebuffers (bug manifested as video feedback when playing with alpha)

0.4.0.1:

- bug fixes / changes relating to interaction between different video outputs (ie. RGBA - RGB - Green - Alpha)

0.4.0.0:

- significant optimizations of fragment shader generation (with more to come!)
- many operations now expand multichannel signals in a combinatorial way (eg. [1,2] + [10,20] === [11,21,12,22])
- earlier semantics (pairwise combination) still available through specialized operators (eg. instead of + use +:, [1,2] +: [10,20] === [11,22])
- added ++ operator to append multi-channel graphs/signals
- order of arguments changed for lpf, bpf, hpf so that controls come first, "audio input" last
- added zero/zer0 function
- added Haskell-style reverse application operator (&)
- added img and vid (deprecating tex, deleting texhsv), setfx, setfy, and setfxy
- added aspect and fit
- added blend
- initial implemention of RGBA output

0.3.4:

- support for multichannel audio

0.3.3:

- support for Haskell-style multi-channel ranges of integer constants [1..6] and float/double constants [0.1,0.2 .. 0.9]

0.3.2.1:

- bug fix relating to line, iline

0.3.2:

- new: added tile, zoom, move, spin (graphics only)

0.3.1:

- new: added 'delay' for delay lines (audio only)

0.3:

- new: audioin for direct use of audio input in audio rendering
- also: initial move to Haskell style version numbering

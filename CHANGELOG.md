# ChangeLog

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

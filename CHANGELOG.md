# ChangeLog

0.4.0.0:
- significant optimizations of fragment shader generation
- many operations now expand multichannel signals in a combinatorial way (eg. [1,2] + [10,20] === [11,21,12,22])
- earlier semantics (pairwise combination) still available through specialized operators (eg. instead of + use |+|, [1,2] |+| [10,20] === [11,22])
- order of arguments changed for lpf, bpf, hpf so that controls come first, "audio input" last

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

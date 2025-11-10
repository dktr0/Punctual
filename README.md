# Punctual

Punctual is a language for live coding audio and visuals. It allows you to build and change networks of signal processors (oscillators, filters, etc) on the fly. When definitions are changed, when and how they change can be explicitly indicated.

Punctual runs in a web browser, and is portable to any system with a browser that supports the Web Audio API (for sound) and WebGL (for video). While it can be used in a standalone way, it is also bundled inside the Estuary platform for collaborative live coding.

The easiest way to try Punctual is to point your browser to https://dktr0.github.io/Punctual/ - you will find there a Punctual editor ready to go with no download or installation required. Another way to start using Punctual is via the main Estuary server at https://estuary.mcmaster.ca (select Solo, then in one of the text editor panels select Punctual from the panel's drop down language selection menu). Using Punctual via Estuary makes it possible both to use it alongside other languages supported by Estuary, as well as to collaborate online with other artists. (It is also possible to download a Punctual release for offline use.) To run locally Punctual, download the latest release and start it from a terminal with `npm run serve` and open up http://127.0.0.1:8080 in your browser.

Punctual was created and is maintained by David Ogborn, as a solo art project. Use and discussion of it by others is welcomed (however, contributions/commits to its source code are not). Conceptually and most immediately, Punctual extends the work of Julian Rohrhuber and others on SuperCollider's JITlib notations, as well as the work of Shawn Lawson on The Force. As of version 0.5, Punctual is built as an Estuary exolang, which means that updated versions of it can be used in the collaborative context of Estuary quite readily, and also that it exists as a JavaScript library that could (in theory) be used elsewhere.

The official and (usually) complete list of Punctual's functions is at [REFERENCE.MD](REFERENCE.md). See also [CHANGELOG.md](CHANGELOG.md) for a history of changes. Those seeking detailed learning materials about using Punctual for visuals are recommended to consult Joan Queralt Molina's excellent [[C]omplete guide to live-coding visuals in Punctual](https://punctual.savamala.top/). Bernard Cleary's wonderful materials for [Decoded -- An Artistic Livecoding Workshop](https://decoded.livecode.au/#/) include a gentle introduction to live-coding visuals with Punctual in the setting of the Estuary collaborative livecoding platform. Discussion and assistance about Punctual can be found in the #punctual channel of [the Estuary discord server](https://bit.ly/EstuaryDiscord) - please don't hesitate to ask whatever you are wondering about there!

# Audio Output

```
osc 440 >> audio; -- a 440 Hz sine-wave panned to the centre
osc [440,550] >> audio; -- two sine-waves panned hard left and right
osc [440,550,660,770] >> audio; -- four sine-waves spread over the panning space
osc 440; -- no audible output
```

# Video Output

```
[1,0,0] >> add; -- a very red screen
[0,1,0] >> add; -- a very green screen
[0,0,1] >> add; -- a very blue screen
[osc 0.2,0,0] >> add; -- a pulsating red screen (modulated by a sine wave)
[unipolar $ osc 0.2, 0, 0] >> add; -- using all of the sine wave's range for the colour
[unipolar fx,0,0] >> add; -- getting redder as we go from left to right
[0,unipolar fy,0] >> add; -- getting greener as we go from top to bottom
```

Punctual video output is layered, like layers in image editing/compositing software. The 'add' output in the above examples expects 3 channels (or multiples of 3 channels) worth of information and adds them, successively, to the red green and blue channels of the previous layer. Other outputs include 'blend' which uses an alpha channel to specify how the new layer should be combined with previous layers (4 channels = red green blue alpha), 'mul' (3 channels, red green blue that are multiplied by previous layers), 'rgb' (3 channels = red green blue, replaces previous output completely), and 'rgba' (4 channels = red green blue alpha, replaces previous output completely).

# Oscillators and Filters

```
osc 440 >> audio; -- a 440 Hz sine wave
tri 440 >> audio; -- a 440 Hz band-limited triangle wave
sqr 440 >> audio; -- a 440 Hz band-limited square wave
saw 440 >> audio; -- a 440 Hz band-limited sawtooth wave
osc 440 * lftri 1 >> audio; a 440 Hz sine wave modulated by a NOT-band-limited triangle wave
osc 440 * lfsqr 1 >> audio; a 440 Hz sine wave modulated by a NOT-band-limited square wave
osc 440 * lfsaw 1 >> audio; a 440 Hz sine wave modulated by a NOT-band-limited saw wave
lpf 1000 1 (saw 110); -- a 1000 Hz (Q=1) low-pass filter applied to a sawtooth wave
hpf 1000 1 (saw 110); -- a 1000 Hz (Q=1) high-pass filter applied to a sawtooth wave
bpf 1000 1 (saw 110); -- a 1000 Hz (Q=1) band-pass filter applied to a sawtooth wave
```

# MIDI note numbers and Decibels

When working with musical pitch and loudness, it is often more intuitive to express
the frequency of things in MIDI note numbers (where an increase of one is equivalent
  to one musical semitone) and to express the amplitude of things in decibels (where
    an increase of six is roughly equivalent to doubling something).

```
osc (midicps 57) >> audio; -- a 440 Hz sine wave / midi note 57 / "middle A"
osc (midicps 57.1) >> audio; -- a slightly out of tune "middle A"
osc (midicps 57) * dbamp (-10) >> audio; -- a quieter sine wave
osc (midicps 57) * dbamp (-13) >> audio; -- quieter still...
osc (midicps 57) * dbamp (-40) >> audio; -- much quieter
```

# Crossfades and quantization

By default, when definitions change the new version of the definition begins to take
effect on the next cycle boundary in the current musical tempo, and there is a brief
crossfade between the old and new definitions. This default helps old and new things
tend towards alignment in time, and avoids clicks and pops. Often, more control over
this replacement process is desired:

```
osc 440 >> audio <> 8; -- when changed and reevaluated, crossfades over 8 seconds
```

# Modulated Ranges and Percentages

Punctual's oscillators give results in the range -1 to 1. It is very common to need
to rescale that range to another range - for example, when using one oscillator to
control the frequency of another, or to control the cutoff frequency of a filter, etc.
Modulated ranges are a series of Punctual specific notations for this common
mapping/scaling operation:

```
saw (midicps $ 24 +- 0.03 $ osc 1) >> audio; -- go between 3% below MIDI note 24 and 3% above, driven by a 1 Hz sine wave
lpf (100 ~~ 1000 $ osc 1) (saw $ midicps 24)  >> audio; -- filter frequency from 100 to 1000, driven by a 1 Hz sine wave
saw (midicps $ 24 +- 0.03 $ osc 1 * sqr 2) >> audio; -- using a more complex "driver" for the modulation
```

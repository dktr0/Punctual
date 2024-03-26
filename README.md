# Punctual

Punctual is a language for live coding audio and visuals. It allows you to build
and change networks of signal processors (oscillators, filters, etc) on the fly.
When definitions are changed, when and how they change can be explicitly indicated.

Punctual runs in a web browser, and is portable to any system with a browser that
supports the Web Audio API (for sound) and WebGL (for video). While it can be used in a standalone way, it is also bundled inside the Estuary platform for collaborative live coding.

The easiest way to try Punctual is to point your browser to https://dktr0.github.io/Punctual/ - you will find there a Punctual editor ready to go with no download or installation required. Another way to start using Punctual is via the main Estuary server at https://estuary.mcmaster.ca (select Solo, then in one of the text editor panels select Punctual from the panel's drop down language selection menu). Using Punctual via Estuary makes it possible both to use it alongside other languages supported by Estuary, as well as to collaborate online with other artists. (It is also possible to download a Punctual release for offline use.)

Punctual was created by David Ogborn. Conceptually and most immediately, Punctual extends the work of Julian Rohrhuber and others on SuperCollider's JITlib notations, as well as the work of Shawn Lawson on The Force.

See also [REFERENCE.MD](REFERENCE.md) for what should be an up-to-date list of Punctual's functions, and [CHANGELOG.md](CHANGELOG.md) for a history of changes.

# Audio Output

```
osc 440 >> audio; -- sound panned to the centre
osc 440 >> centre; -- sound also panned to the centre
osc 440 >> 0.5; -- also panned to the centre
osc 440 >> left;
osc 440 >> right;
osc 440; -- no audible output
```

# Video Output

Video output with Punctual is a matter of directing signals to targets for the red, green, and blue outputs of a "fragment shader" (this is similar to the type of programming one
sees in the environment "The Force" - except that Punctual's notations are are turned into
a fragment shader instead of programming the shader directly in GLSL).

There are three colour targets - red green blue - and they each respond to values in the range from 0 (darkest) to 1 (brightest). This is different than the usual range for audio signals (-1 to 1). Punctual provides two functions for rescaling between these two ranges. The function unipolar expects a signal from -1 to 1 and gives back a signal from 0 to 1. The function bipolar expects a signal from 0 to 1 and gives back a signal from -1 to 1.

The functions fx and fy represent the position of the current "fragment" (ie. pixel) that is being drawn, as a range from -1 (bottom or left) to +1 (top or right). (Note: when fx and fy are used in expressions targeting sound output, they are constant signals of +1).

```
1 >> red; -- a very red screen
osc 0.2 >> red; -- a pulsating red screen
unipolar (osc 0.2) >> red; -- using all of the sine wave's range for the colour
unipolar (osc 0.2) * (-10) db >> red; -- a bit darker
fx >> red; -- getting redder as we go from left to right
fy * (-1) >> green; -- getting greener as we go from to bottom
osc (fx * 60m) * osc (fy * 60.05m) * fx * fy * 10db >> blue; -- pretty patterns
```

# Oscillators and Filters

```
osc 440; -- a 440 Hz sine wave
tri 440; -- a 440 Hz triangle wave
sqr 440; -- a 440 Hz square wave
saw 440; -- a 440 Hz sawtooth wave
lpf (saw 110) 1000 1; -- a 1000 Hz (Q=1) low-pass filter applied to a sawtooth wave
hpf (saw 110) 1000 1; -- a 1000 Hz (Q=1) high-pass filter applied to a sawtooth wave
```

# MIDI note numbers and Decibels

When working with musical pitch and loudness, it is often more intuitive to express
the frequency of things in MIDI note numbers (where an increase of one is equivalent
  to one musical semitone) and to express the amplitude of things in decibels (where
    an increase of six is roughly equivalent to doubling something).

```
osc (57m); -- also a 440 Hz sine wave, expressed in MIDI note numbers (57m = 440)
osc (57.1m); -- a slightly out of tune 440 Hz sine wave
osc (57m) * (-10) db; -- a quieter sine wave
osc (57m) * (-13) db; -- quieter still...
osc (57m) * (-40) db; -- much quieter
```

Note in the last few example aboves that the 57m "associates" with the "sin" rather
than with the * -40 db - so we get a 440 Hz sine wave whose output is then made quieter
by being multiplied by -40 dB. If instead we wanted to multiply the number used as the
frequency of the oscillator we'd use brackets like this:

```
osc (57m * 0.5); -- frequency is half of the frequency corresponding to 57m
```

# Crossfades and quantization

By default, when definitions change the new version of the definition begins to take
effect on the next cycle boundary in the current musical tempo, and there is a brief
crossfade between the old and new definitions. This default helps old and new things
tend towards alignment in time, and avoids clicks and pops. Often, more control over
this replacement process is desired:

```
osc 440 >> centre <> 8s; -- new definition crossfades over 8 seconds
osc 440 >> centre <> 2500ms; -- crossfade over 2500 milliseconds
osc 440 >> centre <> 1.5c; -- crossfade over one and a half cycles (bars)
```

Note that in each of the above example lines, you'll have to change the definition
to be able to hear the effect of the crossfades and quantization.

# Modulated Ranges and Percentages

Punctual's oscillators give results in the range -1 to 1. It is very common to need
to rescale that range to another range - for example, when using one oscillator to
control the frequency of another, or to control the cutoff frequency of a filter, etc.
Modulated ranges are a series of Punctual specific notations for this common
mapping/scaling operation:

```
saw (midicps $ 24 +- 0.03 $ osc 1) >> centre; -- go between 3% below MIDI note 24 and 3% above, driven by a 1 Hz sine wave
lpf (saw $ 24m) (100 ~~ 1000 $ osc 1) 1 >> centre; -- filter frequency from 100 to 1000, driven by a 1 Hz sine wave
saw (midicps $ 24 +- 0.03 $ osc 1 * sqr 2) >> centre; -- using a more complex "driver" for the modulation
```

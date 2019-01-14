# Punctual

Punctual is a language for live coding audio and visuals. It allows you to build
and change networks of signal processors (oscillators, filters, etc) on the fly.
When definitions are changed, when and how they change can be explicitly indicated.

Punctual runs in a web browser, and is portable to any system with a browser that
supports the Web Audio API. While it can be used in a standalone way, it is also
bundled inside the Estuary platform for collaborative live coding. The easiest way to get
started with Punctual is to use the Estuary test server at http://intramuros.mcmaster.ca:8002 (note: Chrome is *strongly* recommended. select Solo, then in one of the text editor panels select PunctualAudio or PunctualVideo from the panel's drop down language selection menu). 

Punctual was created by David Ogborn, building on top of the MusicW synthesis
library (by David Ogborn, Spencer Park, Jamie Beverley, and others). Conceptually,
Punctual extends the work of Julian Rohrhuber and others on SuperCollider's JITlib notations.

# Audio Output

```
sin 440 => centre; -- sound panned to the centre
sin 440 => 50%; -- also panned to the centre
sin 440 => 0.5; -- also panned to the centre
sin 440 => left;
sin 440 => right;
sin 440 => 25%; --panned halfway to the left
sin 440; -- no audible output
```

# Video Output

Video output with Punctual is a matter of using signal processors to direct a
drawing element around a canvas (a kind of low frequency video synthesis). Each
video parameter ranges from -1 (minimum) to 1 (maximum). This is the same as
the output range of the oscillators (sin, etc) built in to Punctual. Here is an
example showing all video parameters used together:

```
sin 0.5 => x;
sin 0.2 => y;
0 => red; -- 50% red
-1 => green; -- 0% green;
-1 => blue; -- 0% blue;
0.5 => alpha; -- 75% opaque drawing
-1 => clear; -- don't erase the canvas at all, each frame
-0.5 => width; -- make the
-0.5 => height;
```

# Oscillators and Filters

```
sin 440; -- a 440 Hz sine wave
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
sin 57m; -- also a 440 Hz sine wave, expressed in MIDI note numbers (57m = 440)
sin 57.1m; -- a slightly out of tune 440 Hz sine wave
sin 57m * -10 db; -- a quieter sine wave
sin 57m * -13 db; -- quieter still...
sin 57m * -40 db; -- much quieter
```

Note in the last few example aboves that the 57m "associates" with the "sin" rather
than with the * -40 db - so we get a 440 Hz sine wave whose output is then made quieter
by being multiplied by -40 dB. If instead we wanted to multiply the number used as the
frequency of the oscillator we'd used brackets like this:

```
sin (57m * 0.5); -- frequency is half of the frequency corresponding to 57m
```

# Crossfades and quantization

By default, when definitions change the new version of the definition begins to take
effect on the next cycle boundary in the current musical tempo, and there is a brief
crossfade between the old and new definitions. This default helps old and new things
tend towards alignment in time, and avoids clicks and pops. Often, more control over
this replacement process is desired:

```
<8s> sin 440 => centre; -- new definition crossfades over 8 seconds
<2500ms> sin 440 => centre; -- crossfade over 2500 milliseconds
<1.5c> sin 440 => centre; -- crossfade over one and a half cycles (bars)
@4c sin 440 => centre; -- new definition starts on next 4-cycle/bar boundary
@0.5c sin 440 => centre; -- new definition starts on next half cycle boundary
@(2c,0.5c) sin 440 => centre; -- new definition starts half cycle after next two cycle boundary
@2c <2c> sin 440 => centre; -- new def starts at next 2-cycle boundary, crossfades over 2 cycles
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
saw (24m +- 3% : sin 1) => centre; -- go between 3% below MIDI note 24 and 3% above, driven by a 1 Hz sine wave
lpf (saw 24m) (100 .. 1000 : sin 1) 1 => centre; -- filter frequency from 100 to 1000, driven by a 1 Hz sine wave
sin (440 : sin 1) => centre; -- sine wave frequency goes between 0 and 440, driven by a 1 Hz sine wave
saw (24m +- 3% : (sin 1 * sqr 2)) => centre; -- using a more complex "driver" for the modulation
saw ((24m .. 30m : sqr 2) +- 3% : sin 1) => centre; -- the elements of the ranges can be more complex "graphs" or nested modulated ranges as well
```

# Future Work

At the time of writing, the next major anticipated feature of Punctual is a kind of variable
system allowing synthesis graphs to be used inside other synthesis graphs. It will
probably work like this:

```
a <2s> 24m +- 3% : saw 1; -- a is approximately 24m (+- 3% according 1 Hz saw modulation)
<8s> tri a * -10db => centre; -- the frequency of this triangle wave is controlled by a
<8s> saw a * -10db => centre; -- the frequency of this sawtooth wave is also controlled by a
```

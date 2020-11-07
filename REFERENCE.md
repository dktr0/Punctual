# Punctual Reference

## Punctual Oscillators, Filters, and Noise

sin [freq] -- sine wave, ranging from 1 to -1

tri [freq] -- triangle wave (in audio rendering, this will be band-limited - see also lftri, below)

saw [freq] -- sawtooth wave (in audio rendering, this will be band-limited - see also lfsaw, below)

sqr [freq] -- square wave (in audio rendering, this will be band-limited - see also lfsqr, below)

lftri [freq] -- "low frequency" triangle wave which unlike tri is not band-limited (in audio), and which goes from -1 to 1. In WebGL graphics output, tri and lftri are identical.

lfsaw [freq] -- "low frequency" sawtooth wave which unlike saw is not band-limited (in audio), and which goes from -1 to 1. Likely useful as a phasor. In WebGL graphics output, saw and lfsaw are identical.

lfsqr [freq] -- "low frequency" square wave which unlike sqr is not band-limited (in audio), and which goes from -1 to 1. In WebGL graphics output, sqr and lfsqr are identical.

lpf [input] [freq] [Q] -- lowpass filter at specified filter and Q

hpf [input] [freq] [Q] -- highpass filter at specified filter and Q

bpf [input] [freq] [Q] -- bandpass filter at specified filter and Q

rnd -- random, "white" noise ranging from -1 to 1

(Note: filters and noise are not implemented in graphics (WebGL) implementation yet.)

## Punctual Functions related to Time and Tempo

These functions are currently graphics (WebGL) only, and will return 0 when translated to audio (an audio implementation is planned, however).

cps -- equivalent to the current tempo in cycles per second (in the standalone version of Punctual this will always be 0.5, but when Punctual is used inside Estuary it will be whatever the tempo has been set to).

time -- how much time in seconds has passed since "beat 0" of the tempo (in the standalone version of Punctual beat 0 is when you load the web page; in Estuary beat 0 can be anytime in history, but is usually the time at which a collaborative ensemble was created).

beat -- how many beats have passed since beat 0 of the tempo

etime -- how much time in seconds has passed since code was last evaluated

ebeat -- how much time has passed since code was last evaluated, expressed in beats/cycles relative to the current tempo

## Punctual Graph Functions

[graph] * [graph] -- multiplication

[graph] / [graph] -- "safe" division, where dividing by 0 yields a result of 0

[graph] + [graph] -- addition

[graph] - [graph] -- subtraction

[graph] ** [graph] -- exponentiation, ie. x to the power of y

[graph] > [graph] -- greater than (1 = true, 0 = false)

[graph] >= [graph] -- greater than or equal (1 = true, 0 = false)

[graph] < [graph] -- less than (1 = true, 0 = false)

[graph] <= [graph] -- less than or equal (1 = true, 0 = false)

[graph] == [graph] -- equal to (1 = true, 0 = false)

[graph] != [graph] -- not equal to (1 = true, 0 = false)

max [graph] [graph] -- returns the maximum value from two graphs

min [graph] [graph] -- returns the minimum value from two graphs

abs [graph] -- absolute value of provided graph

floor [graph] -- the first whole number below the value of the argument, eg. given 2.3 the return value would be 2.0

ceil [graph] -- the first whole number above the value of the argument, eg. given 2.3 the return value would be 3.0

fract [graph] - the fractional part of the argument, eg. given 2.3 the return value would be 0.3

cpsmidi [graph] -- the MIDI note number corresponding to the provided frequency in Hertz

midicps [graph] -- the frequency in Hertz corresponding to the provided MIDI note number

dbamp [graph] -- the raw amplitude corresponding to the provided value in decibels

ampdb [graph] -- the value in decibels corresponding to the provided raw amplitude value

linlin [min1,max1,...] [min2,max2,...] [input] -- input graph is linearly scaled such that the range (min1,max1) becomes the range (min2,max2)

unipolar [graph] -- input is rescaled as if input range was bipolar (-1,1) and output range unipolar (0,1)

bipolar [graph] -- input is rescaled as if input range was unipolar (0,1) and output range bipolar (-1,1)

[min] ~~  [max] $ [input] -- bipolar (-1,1) input rescaled to range (min,max)

[centre] +- [offsetRatio] [input] -- bipolar (-1,1) input rescaled to range centre +- (offsetRatio * centre), e.g. a +- 0.5 ranges from 0.5a to 1.5a

step [graph,graph,graph,...] [graph] -- given a list of graphs and a second, final, "modulating" graph, output the value of a selected graph from the list according to the second argument (drive with lfsaw to produce a simple step sequencer-like behaviour).

sqrt [graph] -- returns the square root of the graph

mono [graph] -- takes multi-channel graphs down to a single channel by summing/mixing

gate [graph] [graph] -- when the absolute value of the second graph is lower than the absolute value of the first graph the output is zero, otherwise the output is just the value of the second graph (note: unlike a typical audio noise gate this gate closes and opens immediately)

## Punctual Graph Functions Specialised for Graphics

These functions are specialized for graphics. (While they are still "valid" in
audio contexts - so that audio and video outputs can be freely mixed within the same Punctual "program" - in the audio domain these functions will all produce a constant signal of 0.)

fx -- the position of the current fragment along the x-axis from left (-1) to right (1)

fy -- the position of the current fragment along the y-axis from bottom (-1) to top (1)

fxy -- the position of the current fragment along the x and y axes as a 2-channel signal

px -- a nominal value for the width of a pixel

py -- a nominal value for the height of a pixel

dist [x,y,...] -- the distance from specified position to current fragment

prox [x,y,...] -- the "proximity" of specified position to current fragment; equivalent to (2.828427-dist[x,y,...])/2.828427, clamped to be between 0 and 1 (2.828427 is maximum on-screen distance)

circle [x,y,...] [r] -- returns 1 when current fragment within a circle at x and y with radius r

point [x,y,...] -- returns 1 when current fragment is within a pixel of x and y, 0 otherwise

rect [x,y,...] [w,h,...] -- returns 1 when current fragment is within rectangle (x and y are centre not corner), 0 otherwise

hline [y] [w] -- returns 1 when current fragment is within w of a horizontal line at y, 0 otherwise

vline [x] [w] -- returns 1 when current fragment is within w of a vertical line at x, 0 otherwise

iline [x1,y1,...] [x2,y2,...] [w] -- returns 1 when current fragment is within w of an infinite line that passes through x1,y1 and x2,y2; otherwise 0

line [x1,y1,...] [x2,y2,...] [w] -- returns 1 when current fragment is within w of a line that goes from x1,y1 and x2,y2; otherwise 0

tex "https://url-to-image-file" [x,y,...] -- accesses a texture built from the image file in question as red-green-blue (3-channel signal)

texhsv "https://url-to-image-file" [x,y,...] -- accesses a texture built from the image file in question as hue-saturation-value (3-channel signal)

hi -- analysis of how much power there is in the highest frequencies of the spectrum of the current sound output. Result is unipolar (0 to 1).

mid -- analysis of how much power there is in the middle frequencies of the spectrum of the current sound output. Result is unipolar (0 to 1).

lo -- analysis of how much power there is in the lowest frequencies of the spectrum of the current sound output. Result is unipolar (0 to 1).

ihi -- analysis of how much power there is in the highest frequencies of the spectrum of the current sound input. Result is unipolar (0 to 1).

imid -- analysis of how much power there is in the middle frequencies of the spectrum of the current sound input. Result is unipolar (0 to 1).

ilo -- analysis of how much power there is in the lowest frequencies of the spectrum of the current sound input. Result is unipolar (0 to 1).

fft [graph] -- detailed spectrum analysis (Fast Fourier Transform) of the current sound output. Graph argument is bipolar (-1 to 1) where -1 represents lowest possible frequency and 1 represents highest. Result is unipolar (0 to 1).

ifft [graph] -- detailed frequency analysis (Fast Fourier Transform) of the current sound input. Graph argument is bipolar (-1 to 1) where -1 represents lowest possible frequency and 1 represents highest. Result is unipolar (0 to 1.)

rgbhsv [r,g,b,...] -- convert every 3 channels of red-green-blue signal to hue-saturation-value

hsvrgb [h,s,v,...] -- convert every 3 channels of hue-saturation-value signal to red-green-blue

rgbh [r,g,b,...] -- convert every 3 channels of red-green-blue signal to 1 channel of hue

rgbs [r,g,b,...] -- convert every 3 channels of red-green-blue signal to 1 channel of saturation

rgbv [r,g,b,...] -- convert every 3 channels of red-green-blue signal to 1 channel of value

rgbr [r,g,b,...] -- convert every 3 channels of red-green-blue signal to 1 channel of red (ie. drop 2nd and 3rd channels)

rgbg [r,g,b,...] -- convert every 3 channels of red-green-blue signal to 1 channel of green (ie. drop 1st and 3rd channels)

rgbb [r,g,b,...] -- convert every 3 channels of red-green-blue signal to 1 channel of blue (ie. drop 1st and 2nd channels)

hsvh [r,g,b,...] -- convert every 3 channels of hue-saturation-value signal to 1 channel of hue (ie. drop 2nd and 3rd channels)

hsvs [r,g,b,...] -- convert every 3 channels of hue-saturation-value signal to 1 channel of saturation (ie. drop 2nd and 3rd channels)

hsvv [r,g,b,...] -- convert every 3 channels of hue-saturation-value signal to 1 channel of value (ie. drop 2nd and 3rd channels)

hsvr [r,g,b,...] -- convert every 3 channels of hue-saturation-value signal to 1 channel of red

hsvg [r,g,b,...] -- convert every 3 channels of hue-saturation-value signal to 1 channel of green

hsvb [r,g,b,...] -- convert every 3 channels of hue-saturation-value signal to 1 channel of blue


## Punctual Output Notations

A Punctual statement does not cause audio or video output unless it ends with >> and an output notation. 'audio' and 'video' are the two most commonly used outputs:

\>> audio -- audio output: If multiple channels of audio are present, they are spread/panned "equidistantly" over the available audio outputs. (deprecated: this was formerly known as 'splay')

\>> video -- video output: every three channels of signal are interpreted as red, green, and blue intensities (from 0 to 1); if only a one-channel signal is provided the value of that signal is used for all of red, green, and blue intensities; if a two-channel signal is provided the first channel is used for red and green, and the second channel is used for blue. (note: 'rgb' is a synonym for 'video')

There are a number of additional output types for particular purposes, as follows:

\>> left -- audio output panned to the left (ie. to the first audio output connected to the system). If multiple channels are present, they are mixed together before being sent to the output.

\>> right -- audio output panned to the right (ie. to the second audio output connected to the system). If multiple channels are present, they are mixed together before being sent to the output.

\>> centre -- audio output panned to the centre (ie. equal power panned between the first two connected audio outputs). If multiple channels are present, they are mixed together before being sent to the output.

\>> [number between 0 and 1] -- audio output panned somewhere between 1st audio output (0) and 2nd audio output (1). If multiple channels are present, they are mixed together before being sent to the output.

\>> red -- intensity of red colour (0 to 1). If multiple channels are present, they are mixed together before being sent to the output.

\>> green -- intensity of green colour (0 to 1). If multiple channels are present, they are mixed together before being sent to the output.

\>> blue -- intensity of blue colour (0 to 1). If multiple channels are present, they are mixed together before being sent to the output.

\>> alpha -- when not specified alpha defaults to 1, which will erase (overwrite) any previous/underlying layers of drawing in circumstances where "this" Punctual program is drawing after/over other layers of drawing (for example: multiple Punctual programs in different zones of an Estuary collaborative interface). 0 for alpha values will not erase/overwrite previously drawn layers - instead the previous intensities will be kept at their pre-existent level and newly provided intensities will be added.

\>> hsv -- every three channels of signal are interpreted as hue, saturation, and value intensities. (Note: one does not need to use an hsv output in order to work with colour from an HSV standpoint - one can also use the built-in functions 'rgbhsv' and 'hsvrgb' to go back and forth between RGB and HSV colour models, using 'video' [RGB] for the final output.)

\>> hue

\>> saturation

\>> value

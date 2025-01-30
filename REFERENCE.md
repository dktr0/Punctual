# Punctual Reference

## Punctual Oscillators, Filters, Noise, Audio Input

osc [freq] -- sine wave oscillator, ranging from 1 to -1 (note: calling this sin is deprecated, see also sin' below; as of Punctual 0.5, only osc will produce a sine oscillator (as opposed to sine function))

tri [freq] -- triangle wave oscillator (in audio rendering, this will be band-limited - see also lftri, below)

saw [freq] -- sawtooth wave oscillator(in audio rendering, this will be band-limited - see also lfsaw, below)

sqr [freq] -- square wave oscillator (in audio rendering, this will be band-limited - see also lfsqr, below)

lftri [freq] -- "low frequency" triangle wave which unlike tri is not band-limited (in audio), and which goes from -1 to 1. In WebGL graphics output, tri and lftri are identical.

lfsaw [freq] -- "low frequency" sawtooth wave which unlike saw is not band-limited (in audio), and which goes from -1 to 1. Likely useful as a phasor. In WebGL graphics output, saw and lfsaw are identical.

lfsqr [freq] -- "low frequency" square wave which unlike sqr is not band-limited (in audio), and which goes from -1 to 1. In WebGL graphics output, sqr and lfsqr are identical.

lpf [freq] [Q] [input] -- lowpass filter at specified filter and Q, combinatorial semantics

lpfp [freq] [Q] [input] -- lowpass filter at specified filter and Q, pairwise semantics

hpf [freq] [Q] [input] -- highpass filter at specified filter and Q, combinatorial semantics

hpfp [freq] [Q] [input] -- highpass filter at specified filter and Q, pairwise semantics

bpf [freq] [Q] [input] -- bandpass filter at specified filter and Q, combinatorial semantics

bpfp [freq] [Q] [input] -- bandpass filter at specified filter and Q, pairwise semantics

rnd -- random, "white" noise ranging from -1 to 1

audioin -- input from Punctual's audio input (normally, the "microphone"; audio only, equivalent to 0 in fragment shaders)

delay [maxDelayTime] [delayTimes] [signals] -- audio delay line, delay signals by specified delay times which must be less than maxDelayTime (audio only, equivalent to 0 in fragment shaders), combinatorial semantics

delayp [maxDelayTime] [delayTimes] [signals] -- audio delay line, delay signals by specified delay times which must be less than maxDelayTime (audio only, equivalent to 0 in fragment shaders), pairwise semantics

(Note: filters and noise are not implemented in graphics (WebGL) implementation yet.)

## Punctual Functions related to Time and Tempo

These functions are currently graphics (WebGL) only, and will return 0 when translated to audio (an audio implementation is planned, however).

cps -- equivalent to the current tempo in cycles per second (in the standalone version of Punctual this will always be 0.5, but when Punctual is used inside Estuary it will be whatever the tempo has been set to).

time -- how much time in seconds has passed since "beat 0" of the tempo (in the standalone version of Punctual beat 0 is when you load the web page; in Estuary beat 0 can be anytime in history, but is usually the time at which a collaborative ensemble was created).

beat -- how many beats have passed since beat 0 of the tempo

etime -- how much time in seconds has passed since code was last evaluated

ebeat -- how much time has passed since code was last evaluated, expressed in beats/cycles relative to the current tempo

## Punctual Functions

When Punctual functions or operators take two or more arguments that are, themselves, Punctual graphs, the question arises of how multiple channels from both (or more) arguments are to be combined. Generally speaking, as of version 0.4.x of Punctual, the default answer to that question is 'combinatorial', eg. ```[1,2] + [10,20]``` is equivalent to [11,12,21,22] (every combination of both sets). However, for the most common mathematical operators, "pairwise" equivalents exist as well, denoted by adding a colon : to the right end of the operator, eg. ```[1,2] +: [10,20]``` is equivalent to [11,22].

[graph] + [graph] -- addition (combinatorial, for pairwise use +: )

[graph] - [graph] -- subtraction (combinatorial, for pairwise use -: )

[graph] * [graph] -- multiplication (combinatorial, for pairwise use *: )

[graph] / [graph] -- "safe" division, where dividing by 0 yields a result of 0 (combinatorial, for pairwise use /: )

[graph] % [graph] -- mod/modulo operation, ie. the remainder when dividing x by y (combinatorial, for pairwise use %: )

[graph] ** [graph] -- exponentiation, ie. x to the power of y (combinatorial, for pairwise use **: )

[graph] == [graph] -- equal to (1 = true, 0 = false, combinatorial, for pairwise use ==: )

[graph] /= [graph] -- not equal to (1 = true, 0 = false, combinatorial, for pairwise use /=: )

[graph] > [graph] -- greater than (1 = true, 0 = false, combinatorial, for pairwise use >: )

[graph] >= [graph] -- greater than or equal (1 = true, 0 = false, combinatorial, for pairwise use >=: )

[graph] < [graph] -- less than (1 = true, 0 = false, combinatorial, for pairwise use <: )

[graph] <= [graph] -- less than or equal (1 = true, 0 = false, combinatorial, for pairwise use <=: )

max [graph] [graph] -- returns the maximum value from two graphs (combinatorial)

maxp [graph] [graph] -- returns the maximum value from two graphs (pairwise)

min [graph] [graph] -- returns the minimum value from two graphs (combinatorial)

minp [graph] [graph] -- returns the minimum value from two graphs (pairwise)

between [min1,max1,min2,max2, ...] [x, ...] -- returns 1 (true) if values of x are between ranges specified by min1,max1,min2,max2,etc (combinatorial)

betweenp [min1,max1,min2,max2, ...] [x, ...] -- returns 1 (true) if values of x are between ranges specified by min1,max1,min2,max2,etc (pairwise)

abs [graph] -- absolute value of provided graph

acos [graph] -- area cosine of provided graph

acosh [graph] -- hyperbolic area cosine of provided graph

asin [graph] -- area sine of provided graph

asinh [graph] -- hyperbolic area sine of provided graph

atan [graph] -- area tangent of provided graph

atanh [graph] -- hyperbolic area tangent of provided graph

cbrt [graph] -- cube root of provided graph

ceil [graph] -- the first whole number above the value of the argument, eg. given 2.3 the return value would be 3.0

cos [graph] -- cosine of provided graph

cosh [graph] -- hyperbolic cosine of provided graph

exp [graph] -- the constant e to the power of provided graph

floor [graph] -- the first whole number below the value of the argument, eg. given 2.3 the return value would be 2.0

log [graph] -- the natural logarithm of provided graph

log2 [graph] -- the base 2 logarithm of provided graph

log10 [graph] -- the base 10 logarithm of provided graph

round [graph] -- values in the provided graph are rounded to the nearest whole number

sign [graph] -- the sign of the provided graph (-1 for negative numbers, 0 for zero, 1 for positive numbers)

sin' [graph] -- the sine of the provided graph (note: as of Punctual 0.5 this will be called sin, and the function previously known as sin will only be accessible as osc)

sinh [graph] -- the hyperbolic sine of the provided graph

sqrt [graph] -- returns the square root of the graph

tan [graph] -- the tangent of the provided graph

tanh [graph] -- the hyperbolic tangent of the provided graph

trunc [graph] -- values in the provided graph are truncated to whole numbers by discarding any decimal components

fract [graph] - the fractional part of the argument, eg. given 2.3 the return value would be 0.3

cpsmidi [graph] -- the MIDI note number corresponding to the provided frequency in Hertz

midicps [graph] -- the frequency in Hertz corresponding to the provided MIDI note number

dbamp [graph] -- the raw amplitude corresponding to the provided value in decibels

ampdb [graph] -- the value in decibels corresponding to the provided raw amplitude value

linlin [min1,max1,...] [min2,max2,...] [input] -- input graph is linearly scaled such that the range (min1,max1) becomes the range (min2,max2) (combinatorial, for pairwise use linlinp)

clip [min1,max1,...] [input] -- input values are clipped to stay between min and max (combinatorial, for pairwise use clipp)

smoothstep [lowEdge1,highEdge1,...] [input] -- input values below lowEdge yield 0, above highEdge yield 1, in between they are smoothly interpolated, cf. GLSL's smoothstep function (combinatorial, for pairwise use smoothstepp)

unipolar [graph] -- input is rescaled as if input range was bipolar (-1,1) and output range unipolar (0,1)

bipolar [graph] -- input is rescaled as if input range was unipolar (0,1) and output range bipolar (-1,1)

[min] ~~  [max] $ [input] -- bipolar (-1,1) input rescaled to range (min,max) (combinatorial, for pairwise use ~~:)

[centre] +- [offsetRatio] [input] -- bipolar (-1,1) input rescaled to range centre +- (offsetRatio * centre), e.g. a +- 0.5 ranges from 0.5a to 1.5a (combinatorial, for pairwise use +-:)

spr [graph,graph,graph,...] [graph] -- given a list of graphs and a second, final, "modulating" graph, output the value of a selected graph from the list according to the second argument (drive with lfsaw to produce a simple step sequencer-like behaviour).

seq [graph] -- given a list of graphs, outputs the value of the selected graph in such a way that it's spread over a single cycle of the metre

mono [graph] -- takes multi-channel graphs down to a single channel by summing/mixing

gate [graph] [graph] -- when the absolute value of the second graph is lower than the absolute value of the first graph the output is zero, otherwise the output is just the value of the second graph (note: unlike a typical audio noise gate this gate closes and opens immediately; also, this has combinatorial semantics so if the first graph has x channels and the second graph has y channels, the output will have x*y channels)

gatep [graph] [graph] -- when the absolute value of the second graph is lower than the absolute value of the first graph the output is zero, otherwise the output is just the value of the second graph (note: unlike a typical audio noise gate this gate closes and opens immediately; this variant has pairwise semantics, so if the first graph has x channels and the second graph has y channels, the output will have as many channels as the greatest of x or y)

zero [graph] -- returns a graph that is always 0 regardless of the input graph (useful for quickly silencing/erasing particular lines of code). The synonym 'zer0' is also available.

[graph] ++ [graph] -- appends two graphs to each other in a way that preserves the multiple channels of both graphs. For example if the graph on the left has 3 channels, and the one on the right has 2 channels, then the result will be a 5-channel graph consisting of the 3 channels from the left operand, followed by the 2 channels from the right operand.

pi -- the value of PI (3.1415926535897932384626433832795)

pan/panp [n] [graph] [graph] -- equal-power panning (cosine law) over n (an integer) channels. first graph argument is position(s), second argument is input signals to be panned. works in both audio and visual outputs, FWIW.

splay [n] [graph] -- the channels of the provided graph are spread over n (an integer) output channels, using the same equal-power panning algorithm as pan/panp. also works with both audio and visual outputs.

## Ranges

Punctual has the following two notations for producing lists (one-dimensional matrices) of constant values, modeled on similar functionality found in Haskell and Purescript, but with some specific semantics adapted to the live coding situations in which Punctual is expected to be used.

[x ... y] -- x and y must both be constant integers. result is x and y and every integer in between them. if y < x then results will descend towards y. if the provided values would result in more than 64 results, they are limited to the first 64 results.

[a, b .. c] -- a, b, and c must be constant floating point numbers. result is a, then b, then every value between b and c proceeding by interval of (b-a), until and including c. To account for floating point rounding errors, if the second last value is less than half of (b-a) away from c, it is excluded. the final value in the result is always exactly c (no rounding error). If the provided values would result in more than 64 results, they are limited to the first 64 results.

## Punctual Graph Functions Specialised for Graphics

These functions are specialized for graphics. (While they are still "valid" in
audio contexts - so that audio and video outputs can be freely mixed within the same Punctual "program" - in the audio domain these functions will all produce a constant signal of 0.)

fx -- the position of the current fragment along the x-axis from left (-1) to right (1)

fy -- the position of the current fragment along the y-axis from bottom (-1) to top (1)

fxy -- the position of the current fragment along the x and y axes as a 2-channel signal

frt -- the position of the current fragment in polar coordinates (radius, angle/theta)

fr -- the radius of the position of the current fragment in polar coordinates

ft -- the angle (theta) of the position of the current fragment in polar coordinates

px -- the width of an actually displayed pixel (ie. in terms of Punctual's -1 to 1 geometry)

py -- the height of an actually displayed pixel (ie. in terms of Punctual's -1 to 1 geometry)

aspect -- the aspect ratio of the canvas on which Punctual draws

xyrt [x,y,...] -- convert cartesian coordinates to polar coordinates

xyr [x,y,...] -- convert cartesian coordinates to polar coordinates (radii only)

xyt [x,y,...] -- convert cartesian coordinates to polar coordinates (angles/thetas only)

rtxy [r,t,...] -- convert polar coordinates to cartesian coordinates

rtx [r,t,...] -- convert polar coordinates to cartesian coordinates (x dimension only)

rty [r,t,...] -- convert polar coordinates to cartesian coordinates (y dimension only)

dist [x,y,...] -- the distance from specified position to current fragment

prox [x,y,...] -- the "proximity" of specified position to current fragment; equivalent to (2.828427-dist[x,y,...])/2.828427, clamped to be between 0 and 1 (2.828427 is maximum on-screen distance)

circle [x,y,...] [d] -- returns 1 when current fragment within a circle at x and y with diameter d (combinatorial, for pairwise use circlep)

point [x,y,...] -- returns 1 when current fragment is within approximately half a pixel of x and y, 0 otherwise

rect [x,y,...] [w,h,...] -- returns 1 when current fragment is within rectangle (x and y are centre not corner), 0 otherwise (combinatorial, for pairwise use rectp)

hline [y] [w] -- returns 1 when current fragment is within w of a horizontal line at y, 0 otherwise (combinatorial, for pairwise use hlinep)

vline [x] [w] -- returns 1 when current fragment is within w of a vertical line at x, 0 otherwise (combinatorial, for pairwise use vlinep)

iline [x1,y1,...] [x2,y2,...] [w] -- returns 1 when current fragment is within w of an infinite line that passes through x1,y1 and x2,y2; otherwise 0 (combinatorial, for pairwise use ilinep)

line [x1,y1,...] [x2,y2,...] [w] -- returns 1 when current fragment is within w of a line that goes from x1,y1 and x2,y2; otherwise 0 (combinatorial, for pairwise use linep)

lines [x1,y1,x2,y2,...] [w] -- returns 1 when current fragment is within w of a line that goes from x1,y1 to x2,y2; otherwise 0 (combinatorial, for pairwise use linesp)

ilines [x1,y1,x2,y2,...] [w] -- returns 1 when current fragment is within w of an infinite line that goes from x1,y1 to x2,y2; otherwise 0 (combinatorial, for pairwise use ilinesp)

chain [x1,y1,x2,y2,x3,y3,...] [w] -- returns 1 when current fragment is within w of a line that goes from x1,y1 to x2,y2, then from x2,y2 to x3,y3, etc; otherwise 0 (combinatorial, for pairwise use chainp)

mesh [x1,y1,x2,y2,...] [w] -- returns 1 when current fragment is within w of a mesh of lines that go between every pair of x1,y1 x2,y2 etc; otherwise 0 (combinatorial, for pairwise use meshp)

img "https://url-to-image-file" -- accesses a texture built from the image file in question as red-green-blue (3-channel signal).

vid "https://url-to-video-file" -- accesses a texture built from the video file in question as red-green-blue (3-channel signal).

cam -- accesses a texture built from the webcam as red-green-blue (3-channel signal).

fb [x1,y1,...] -- accesses feedback (Punctual video output from the previous frame) as red-green-blue (3-channel signal per 2 channels of input argument)

tex "https://url-to-image-file" [x,y,...] -- accesses a texture built from the image file in question as red-green-blue (3-channel signal). *Deprecated* (use ```img "url"``` instead).

hi -- analysis of how much power there is in the highest frequencies of the spectrum of the current sound output. Result is unipolar (0 to 1).

mid -- analysis of how much power there is in the middle frequencies of the spectrum of the current sound output. Result is unipolar (0 to 1).

lo -- analysis of how much power there is in the lowest frequencies of the spectrum of the current sound output. Result is unipolar (0 to 1).

ihi -- analysis of how much power there is in the highest frequencies of the spectrum of the current sound input. Result is unipolar (0 to 1).

imid -- analysis of how much power there is in the middle frequencies of the spectrum of the current sound input. Result is unipolar (0 to 1).

ilo -- analysis of how much power there is in the lowest frequencies of the spectrum of the current sound input. Result is unipolar (0 to 1).

fft [graph] -- detailed spectrum analysis (Fast Fourier Transform) of the current sound output. Graph argument is bipolar (-1 to 1) where -1 represents lowest possible frequency and 1 represents highest. Result is unipolar (0 to 1).

ifft [graph] -- detailed frequency analysis (Fast Fourier Transform) of the current sound input. Graph argument is bipolar (-1 to 1) where -1 represents lowest possible frequency and 1 represents highest. Result is unipolar (0 to 1.)

blend [r1,g1,b1,a1,r2,g2,b2,a2,...] -- produces a 4-channel RGBA signal by blending layers represented by sets of 4 channels, using the alpha channel of the second of each set to interpolate between values. If the input does not consist of a multiple of 4 channels, it is extended to the next multiple of 4 channels, in the following way: 1 channel = repeat as RGB, alpha is 1; 2 channels = repeat 2nd channel as B, alpha is 1; 3 channels = those 3 channels, with an alpha of 1.

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

tile [x,y] [...] -- repeat the image x times across the x axis, y times across the y axis (remaps fx & fy)

zoom [x,y] [...] -- zoom in by x across the x axis, y across the y axis (remaps fx & fy)

fit [aspectRatio] [...] -- zoom so that the contents of the second argument fit the canvas maximally assuming the provided aspectRatio, eg. to fit a 16/9 video ```fit (16/9) $ vid "urlToVideo.mov" >> video```

move [x,y] [...] -- move/shift/translate things by x across the x axis, y across the y axis (remaps fx & fy)

spin [amount] [...] -- rotate around [0,0], amount is from 0 - 1 where 1 is all the way around circle (remaps fx & fy)

setfx [x1,x2,...] -- remap fx by setting it to the value of a specific expression

setfy [y1,y2,...] -- remap fy by setting it to the value of a specific expression

setfxy [x1,y1,x2,y2,...] -- remap fx and fy by setting them to the value of a specific expression


## Higher-level operations

[graph] & (graph -> graph) -- reverse application (Haskell-style), applies the argument on the left to complete the function on the right, eg. ```saw 55 & lpf 1000 1 >> audio ``` is equivalent to ```lpf 1000 1 $ saw 55 >> audio```


## Punctual Output Notations

A Punctual statement does not cause audio or video output unless it ends with >> and an output notation.

In the pre-alpha draft of Punctual 0.5, the following outputs are available:

\>> audio -- audio output: If multiple channels of audio are present, they are spread/panned "equidistantly" over all of the available audio outputs.

\>> aout [channelOffset] [numberOfChannels] -- an audio output with [numberOfChannels] directed to the output channels starting at channelOffset. The channels of whatever signal is provided are spread over [numberOfChannels]. For example, aout 2 1 would be a mono (one channel) output going out the 3rd channel of audio output.

\>> stereo - a synonym for "aout 0 2"; the channels of whatever signal is provided are spread/panned equidistantly over the first 2 available outputs (regardless of how many channels of audio output are available).

\>> blend -- video output where every four channels of signal are interpreted as red, green, blue, and alpha channels, with the alpha channel controlling the blending with "previous" visual output (previous = in earlier statements in the same Punctual program), and (assuming it arrives at the final output of the program) also affecting the transparency of Punctual's visual output. Whenever less than 4 channels are available, the missing channels are formed as follows: the 2nd or 3rd channels are repeated to get to 3 channels, and then a default value of 1.0 is inserted for the missing alpha channel.

\>> rgba -- video output where every four channels of signal are interpreted as red, green, blue, and alpha channels. Unlike 'blend' above, with 'rgba' all previous visual output (previous = earlier statements in the same Punctual program) is disregarded - the visual output from that point in the program simply becomes the signal provided to rgba. Whenever less than 4 channels are available, the missing channels are formed as follows: the 2nd or 3rd channels are repeated to get to 3 channels, and then a default value of 1.0 is inserted for the missing alpha channel. (Note: this is very different semantics than the RGBA output had in Punctual 0.4. 'blend' above in Punctual 0.5 is the loose equivalent of 'rgba' in Punctual 0.4.)

\>> add -- video output where every three channels of signal are interpreted as red, green, and blue intensities (from 0 to 1), are added to any previous RGB output (previous = effect of earlier statements in the same Punctual program), clamped to fall between 0 and 1, to form a new, resulting RGB output; if previous output is RGBA, then only the RGB channels of that previous output are used by mul. if only a one-channel signal is provided the value of that signal is used for all of red, green, and blue intensities; if a two-channel signal is provided the first channel is used for red, and the second channel is used for green and blue.

\>> mul -- video output where every three channels of signal are interpreted as red, green, and blue intensities (from 0 to 1), are multiplied by any previous RGB output (previous = effect of earlier statements in the same Punctual program), clamped to fall between 0 and 1, to form a new, resulting RGB output; if previous output is RGBA, then only the RGB channels of that previous output are used by mul. if only a one-channel signal is provided the value of that signal is used for all of red, green, and blue intensities; if a two-channel signal is provided the first channel is used for red, and the second channel is used for green and blue.

\>> rgb -- video output where every three channels of signal are interpreted as red, green, and blue intensities (from 0 to 1). Unlike the output modes above, with 'rgb' all previous visual output (previous = earlier statements in the same Punctual program) is disregarded - the visual output from that point in the program simply becomes the signal provided to rgb. if only a one-channel signal is provided the value of that signal is used for all of red, green, and blue intensities; if a two-channel signal is provided the first channel is used for red, and the second channel is used for green and blue. (Note: this is very different semantics than the rgb output had in Punctual 0.4. 'add' above in Punctual 0.5 is the loose equivalent of 'rgb' in Punctual 0.4.)


In version 0.4 of Punctual, the following outputs are/were available - they are all deprecated and will not be available in Punctual 0.5:

\>> video -- video output: every three channels of signal are interpreted as red, green, and blue intensities (from 0 to 1); if only a one-channel signal is provided the value of that signal is used for all of red, green, and blue intensities; if a two-channel signal is provided the first channel is used for red, and the second channel is used for green and blue. (note: 'rgb' is a synonym for 'video')

\>> left -- audio output panned to the left (ie. to the first audio output connected to the system). If multiple channels are present, they are mixed together before being sent to the output.

\>> right -- audio output panned to the right (ie. to the second audio output connected to the system). If multiple channels are present, they are mixed together before being sent to the output.

\>> centre -- audio output panned to the centre (ie. equal power panned between the first two connected audio outputs). If multiple channels are present, they are mixed together before being sent to the output.

\>> [number between 0 and 1] -- audio output panned somewhere between 1st audio output (0) and 2nd audio output (1). If multiple channels are present, they are mixed together before being sent to the output.

\>> red -- intensity of red colour (0 to 1). If multiple channels are present, they are mixed together before being sent to the output.

\>> green -- intensity of green colour (0 to 1). If multiple channels are present, they are mixed together before being sent to the output.

\>> blue -- intensity of blue colour (0 to 1). If multiple channels are present, they are mixed together before being sent to the output.

\>> alpha -- when not specified alpha defaults to 1, which will erase (overwrite) any previous/underlying layers of drawing in circumstances where "this" Punctual program is drawing after/over other layers of drawing (for example: multiple Punctual programs in different zones of an Estuary collaborative interface). 0 for alpha values will not erase/overwrite previously drawn layers - instead the previous intensities will be kept at their pre-existent level and newly provided intensities will be added.

\>> hsv -- every three channels of signal are interpreted as hue, saturation, and value intensities. (Note: one does not need to use an hsv output in order to work with colour from an HSV standpoint - one can also use the built-in functions 'rgbhsv' and 'hsvrgb' to go back and forth between RGB and HSV colour models, using 'video' [RGB] for the final output.) *Deprecated*: instead, use hsvrgb to convert to rgb and then route to `video`.

\>> hue: *Deprecated*

\>> saturation: *Deprecated*

\>> value: *Deprecated*

\>> fdbk: *Deprecated* A shortcut for providing some simple video feedback in which the output of the previous frame of video is blended with the output of the current frame. Sending 0.5 to fdbk would mix in 50% of the levels from the previous frame in the current frame. 


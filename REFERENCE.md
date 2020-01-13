# Punctual Reference

## Punctual Oscillators and Filters

sin [freq] -- sine wave

tri [freq] -- triangle wave

saw [freq] -- sawtooth wave

sqr [freq] -- square wave

lpf [input] [freq] [ratio] -- lowpass filter at specified filter and bandwidth-ratio

hpf [input] [freq] [ratio] -- highpass filter at specified filter and bandwidth-ratio

(Note: filters are not implemented in graphics (WebGL) implementation yet.)

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

abs [graph] -- absolute value of provided graph

cpsmidi [graph] -- the MIDI note number corresponding to the provided frequency in Hertz

midicps [graph] -- the frequency in Hertz corresponding to the provided MIDI note number

dbamp [graph] -- the raw amplitude corresponding to the provided value in decibels

ampdb [graph] -- the value in decibels corresponding to the provided raw amplitude value

linlin [min1] [max1] [min2] [max2] [input] -- input graph is linearly scaled such that the range (min1,max1) becomes the range (min2,max2)

unipolar [graph] -- input is rescaled as if input range was bipolar (-1,1) and output range unipolar (0,1)

bipolar [graph] -- input is rescaled as if input range was unipolar (0,1) and output range bipolar (-1,1)

[min] .. [max] : [input] -- bipolar (-1,1) input rescaled to range (min,max)

[centre] +- [percent]% : [input] -- bipolar (-1,1) input rescaled to range centre +- percent

mean [graph] [graph] -- returns the mean/average of its two inputs

squared [graph] -- returns graph * graph (the graph, squared)

sqrt [graph] -- returns the square root of the graph

mono [graph] -- takes multi-channel graphs down to a single channel by summing/mixing

## Punctual Graph Functions Specialised for Graphics

These functions are specialized for graphics. However, they are still valid in
audio contexts (where they are equivalent to specific default values).

fx -- the position of the current fragment along the x-axis from left (-1) to right (1)

fy -- the position of the current fragment along the y-axis from bottom (-1) to top (1)

px -- a nominal value for the width of a pixel

py -- a nominal value for the height of a pixel

distance [x] [y] -- the distance from specified position to current fragment

circle [x] [y] [r] -- returns 1 when current fragment within a circle at x and y with radius r

point [x] [y] -- returns 1 when current fragment is within a pixel of x and y, 0 otherwise

rect [x] [y] [w] [h] -- returns 1 when current fragment is within rectangle (x and y are centre not corner), 0 otherwise

hline [y] [w] -- returns 1 when current fragment is within w of a horizontal line at y, 0 otherwise

vline [x] [w] -- returns 1 when current fragment is within w of a vertical line at x, 0 otherwise

iline [x1] [y1] [x2] [y2] -- returns 1 when current fragment is within w of an infinite line that passes through x1,y1 and x2,y2; otherwise 0

line [x1] [y1] [x2] [y2] -- returns 1 when current fragment is within w of a line that goes from x1,y1 and x2,y2; otherwise 0

tex "https://url-to-image-file" [x] [y] -- accesses a texture built from the image file in question

## Punctual Output Notations

A Punctual statement does not cause audio or video output unless it ends with => and an output notation. Here are the available output notations:

=> left -- audio output panned to the left (ie. to the first audio output connected to the system)

=> right -- audio output panned to the right (ie. to the second audio output connected to the system)

=> centre -- audio output panned to the centre (ie. equal power panned between the first two connected audio outputs)

=> [number between 0 and 1] -- audio output panned somewhere between 1st audio output (0) and 2nd audio output (1)

Note: when presented with a multichannel signal for output, the panning outputs above will mix the multichannel signal down to mono before panning it.

=> splay -- multiple channels of audio are spread "equidistantly" over the available audio outputs

=> red -- intensity of red colour (0 to 1)

=> green -- intensity of green colour (0 to 1)

=> blue -- intensity of blue colour (0 to 1)

=> rgb -- up to three channels of signal are interpreted as red, green, and blue intensities (from 0 to 1); if only a one-channel signal is provided the value of that signal is used for all of red, green, and blue intensities; if a two-channel signal is provided the first channel is used for red and blue, and the second channel is used for green.

=> alpha -- when not specified alpha defaults to 1, which will erase (overwrite) any previous/underlying layers of drawing in circumstances where "this" Punctual program is drawing after/over other layers of drawing (for example: multiple Punctual programs in different zones of an Estuary collaborative interface). 0 for alpha values will not erase/overwrite previously drawn layers - instead the previous intensities will be kept at their pre-existent level and newly provided intensities will be added.

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

## Punctual Graph Functions Specialised for Graphics

These functions are specialized for graphics. However, they are still valid in
audio contexts (where they are equivalent to specific default values).

fx -- the position of the current fragment along the x-axis from left (-1) to right (1)

fy -- the position of the current fragment along the y-axis from bottom (-1) to top (1)

px -- a nominal value for the width of a pixel

py -- a nominal value for the height of a pixel

point [x] [y] -- returns 1 when current fragment is within a pixel of x and y, 0 otherwise

hline [y] -- returns 1 when current fragment is within a pixel of a horizontal line at y, 0 otherwise

vline [x] -- returns 1 when current fragment is within a pixel of a vertical line at x, 0 otherwise

rect [x] [y] [w] [h] -- returns 1 when current fragment is within rectangle (x and y are centre not corner), 0 otherwise

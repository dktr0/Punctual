audio input, output, and analysis tests

fft analysis of audio input:
hline ifft 0.002 >> add
-- works in standalone; in Estuary, input doesn't correspond to Punctual Audio Input setting in Settings

fft analysis of audio output:
osc 440 >> audio;
hline fft 0.002 >> add
-- works in standalone; in Estuary, analysis is not sourced from global/summed output as it should be (and what if we wanted to change where it comes from?)

3-band analysis of audio input:
circlep [[-0.5,0,0.5],0] [ilo,imid,ihi*5] >> add
-- works in standalone; in Estuary, input doesn't correspond to Punctual Audio Input setting in Settings 

3-band analysis of audio output:
saw (slow 3 $ seq [120,1200,12000]) >> audio;
circlep [[-0.5,0,0.5],0] [lo,mid,hi*5] >> add
-- works in standalone; in Estuary, analysis is not sourced from global/summed output (and what if we wanted to change where it comes from?)

audio input routed to audio output:
ain 1 0 >> audio
-- works in standalone; in Estuary, input does not correspond to Punctual Audio Input setting in Settings
-- also in Estuary, removing this code and reevaluating leads to continuing audio routing (possibly a problem with zone clearing)


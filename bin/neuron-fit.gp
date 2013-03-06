set terminal pdf mono font '/usr/share/fonts/X11/Type1/lmbx10.pfb'
set termoption dash

set output '4-waveform-sel-0.40-0.3-fit.pdf'
infile='tmp/4-waveform-sel-0.40-0.3-fitness'
set title 'Spike-Train Izzy 4, Waveform, Sel, Mutrate = 0.40, Mutprob = 0.3'
set key inside left top box linetype -1 linewidth 1.000
#set key inside right bottom box linetype -1 linewidth 1.000
set samples 50

set xlabel "Generation"
set ylabel "Fitness"
S = 1
plot infile u ($0):1:2 t "Std.dev" w yerrorlines lt 1 lc rgb 'gray70',\
     infile u ($0):1 t "Avg" w l lt 1 lc rgb 'blue',\
     infile u ($0):3 t "Max" w l lt 1 lc rgb 'web-green', \
     infile u ($0):4 t "Min" w l lt 1 lc rgb 'dark-red'

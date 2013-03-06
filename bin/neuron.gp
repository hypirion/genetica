set terminal pdf mono font '/usr/share/fonts/X11/Type1/lmbx10.pfb'
set termoption dash
set output '4-waveform-sel-0.40-0.3.pdf'
set key inside right top box linetype -1 linewidth 1.000

set title 'Spike-Train Izzy 4, Waveform, Sel, Mutrate = 0.40, Mutprob = 0.3'
set grid xtics lt 0 lw 1 lc 'black'
set grid ytics lt 0 lw 1 lc 'black'
set xlabel "Time (ms)"
set ylabel "Activation-Level (mV)"
set yrange[-100:40]

plot 'tmp/4-waveform-sel-0.40-0.3-train' u 1 t "Generated" w l lt 1 lc rgb 'blue' , \
     'train/izzy-train4-gp.dat' u 1 t "Goal" w l lt 1 lc rgb 'web-green'


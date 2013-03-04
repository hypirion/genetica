set terminal pngcairo mono font '/usr/share/fonts/X11/Type1/lmbx10.pfb'
set termoption dash
set output outfile
set key inside right top box linetype -1 linewidth 1.000

set title 'Neuron-spiking take 912'
set grid xtics lt 0 lw 1 lc 'black'
set grid ytics lt 0 lw 1 lc 'black'
set xlabel "Time"
set ylabel "Voltage"

plot infile1 u 1 t "Generated" w l lt 1 lc rgb 'blue' , \
     infile2 u 1 t "Goal" w l lt 1 lc rgb 'web-green'


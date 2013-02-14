set terminal pdf mono font 'arial,11'
set output 'plot.pdf'
set key inside right top box linetype -1 linewidth 1.000
set title "Maximal fitness for different signature cases, smoothed out"
#set yrange [0:4]
set xlabel "Generation"
set ylabel "Fitness"

S = 1

plot 'tmp/plots/blotto-10-0.8-0.0.dat' u 1:3:(0.3) t "Rf = 0.8, Lf = 0.0" smooth acsplines lw 2 lc rgb 'blue', \
     'tmp/plots/blotto-10-0.0-1.0.dat' u 1:3:(0.3) t "Rf = 0.0, Lf = 1.0" smooth acsplines lw 2  lc rgb 'red', \
     'tmp/plots/blotto-10-0.5-0.0.dat' u 1:3:(0.3) t "Rf = 0.5, Lf = 0.0" smooth acsplines lw 2 lc rgb 'orange'
#     "data.dat" u 1:3 t "Max" w l lt 1 lc rgb 'web-green' axes x1y2

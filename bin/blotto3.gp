set terminal pngcairo mono font 'arial,11'
set output 'a.png'
set key inside right top box linetype -1 linewidth 1.000
set title "40-bit one max"
#set yrange [0:4]
set xlabel "Generation"
set ylabel "Fitness"

S = 1

plot 'tmp/plots/blotto-10-0.8-0.0.dat' u 1:3:(0.3) t "Entropy" smooth acsplines lc rgb 'blue', \
     'tmp/plots/blotto-10-0.0-1.0.dat' u 1:3:(0.3) t "Entropy" smooth acsplines  lc rgb 'red', \
     'tmp/plots/blotto-10-0.5-0.0.dat' u 1:3:(0.3) t "Entropy" smooth acsplines lc rgb 'orange'
#     "data.dat" u 1:3 t "Max" w l lt 1 lc rgb 'web-green' axes x1y2

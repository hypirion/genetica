set terminal pngcairo mono font '/usr/share/fonts/X11/Type1/lmbx10.pfb'
set termoption dash
set output 'graph.png'
set key inside right top box linetype -1 linewidth 1.000
set samples 50
set title "Entropy of run"
set yrange [0:6]
set xlabel "Generation"
set ylabel "Fitness"

S = 1

plot 'data.dat' u 1:5 t "Entropy" w l lt 1
#     "data.dat" u 1:3 t "Max" w l lt 1 lc rgb 'web-green' axes x1y2

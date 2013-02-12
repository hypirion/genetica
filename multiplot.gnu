set terminal pdf mono font '/usr/share/fonts/X11/Type1/lmbx10.pfb'
set termoption dash
set output 'plot.pdf'
set key inside right bottom box linetype -1 linewidth 1.000
set samples 50
set title "40-bit one max, 110 pop, roulette, sigma, full replacement"
set yrange [0:45]
set xlabel "Generation"
set ylabel "Fitness"

S = 1
plot "data.dat" u 1:2:4 t "Average + std.dev" w yerrorlines lt 1 lc rgb 'blue',\
     "data.dat" u 1:6:7 t "Max" w yerrorlines lt 1 lc rgb 'web-green', \
     "data.dat" u 1:8:9 t "Min" w yerrorlines lt 1 lc rgb 'dark-red',\
     40 t "" w l lc rgb 'gray'

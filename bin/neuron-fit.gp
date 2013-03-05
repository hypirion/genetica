set terminal pngcairo mono font '/usr/share/fonts/X11/Type1/lmbx10.pfb'
set termoption dash
set output outfile
set key inside left top box linetype -1 linewidth 1.000
set samples 50
set title "40-bit one max, 120 pop, roulette, fitness, full replacement"
set xlabel "Generation"
set ylabel "Fitness"
S = 1
plot infile u ($0):1:2 t "Avg + std.dev" w yerrorlines lt 1 lc rgb 'blue',\
     infile u ($0):3 t "Max" w l lt 1 lc rgb 'web-green', \
     infile u ($0):4 t "Min" w l lt 1 lc rgb 'dark-red'

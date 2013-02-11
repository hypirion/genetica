set terminal pdf mono font '/usr/share/fonts/X11/Type1/lmbx10.pfb'
set output 'plot.pdf'
#set key inside left top box linetype -1 linewidth 1.000
unset key
set samples 50
set title "40-bit one max"
set xlabel "Generation"
set ylabel "Fitness"
S = 1
plot "data.dat" u 1:2:3 t "Average + std.dev" w yerrorlines lc rgb 'blue', \
     "data.dat" u 1:4 t "Max" w l lc rgb 'web-green', \
     "data.dat" u 1:5 t "Min" w l lc rgb 'dark-red'

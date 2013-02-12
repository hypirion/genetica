#! /bin/sh
N=$1
ITERS=$(seq $N)
make --quiet compile
mkdir -p tmp
for i in $ITERS; do
    bin/genetica.sh 100 120 roulette 30 0.2 fitness\
             full_replacement 100 one_max 40 0.2 0.001 1 0.7\
             > tmp/data$i
done
bin/squish.py tmp/data $N | nl -s " " -b a > data.dat
gnuplot bin/multiplot.gnu

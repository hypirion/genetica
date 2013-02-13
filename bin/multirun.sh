#! /bin/sh
N=$1
ITERS=$(seq $N)
make --quiet compile
mkdir -p tmp/plots
for i in $ITERS; do
    bin/genetica.sh 100 120 roulette 20 0.2 $3\
             $4 $5 one_max 40 0.0 0.01 1 0.6\
             > tmp/$2-$i
done
bin/squish.py tmp/$2- $N | nl -s " " -b a > tmp/$2.dat
gnuplot -e "outfile='$2.png'; infile='tmp/$2.dat';" bin/multiplot.gnu
for i in $ITERS; do
    rm -f tmp/$2-$i
done

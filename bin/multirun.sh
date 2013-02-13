#! /bin/sh
N=$1
ITERS=$(seq $N)
make --quiet compile
mkdir -p tmp/plots
for i in $ITERS; do
    bin/genetica.sh 100 120 roulette 30 0.2 fitness\
             full_replacement 100 one_max 40 $3 $4 1 $5\
             > tmp/$2-$i
done
bin/squish.py tmp/$2- $N | nl -s " " -b a > tmp/$2.dat
gnuplot -e "outfile='$2'; infile='tmp/$2.dat';" bin/multiplot.gnu

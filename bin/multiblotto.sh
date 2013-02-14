#! /bin/sh
N=$1
ITERS=$(seq $N)
make --quiet compile
mkdir -p tmp/plots
for i in $ITERS; do
    bin/genetica.sh 500 20 roulette 30 0.2 sigma\
             generational_mixing 5 blotto $3 $4 $5 0.2 0.01 0.6 \
             > tmp/$2-$i
done
bin/squish.py tmp/$2- $N | nl -s " " -b a > tmp/$2.dat
gnuplot -e "outfile='$2.png'; infile='tmp/$2.dat';" bin/multiblotto.gnu
for i in $ITERS; do
    rm -f tmp/$2-$i
done

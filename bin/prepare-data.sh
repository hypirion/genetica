#! /bin/sh

prefix=t-k
suffix=-e0.1
infile=$prefix$suffix

rm -f tmp/$infile.dat
touch tmp/$infile.dat

for I in $(seq 10 10 110); do
    sed -e "s/^/$I /" tmp/plots/$prefix$I$suffix.dat \
                  >> tmp/$infile.dat
    echo >> tmp/$infile.dat
done

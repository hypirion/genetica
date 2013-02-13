#! /bin/sh

mkdir -p plots
for mp in $(seq 0 0.1 1.0); do
    for mbit in 0.01 0.05 0.1 0.25 0.5; do
        for xo in $(seq 0.5 0.05 1.0); do
            bin/multirun.sh 100 plots/m$mp-mb$mbit-cr$xo $mp $mbit $xo &
            echo -n .
        done
        wait $!
    done
done
echo
echo done!


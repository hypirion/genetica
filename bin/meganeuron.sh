#! /bin/sh

mkdir -p plots
mkdir -p tmp/plots
for T in $(seq 1 4); do
    for Xover in avg sel; do
        for Smd in waveform time interval; do
            for Mutr in $(seq 0.0 0.05 0.2); do
                for Mutp in $(seq 0.1 0.1 0.5); do
#                    echo bin/varneuron.sh $T $Smd $Mutr $Mutp $Xover
                    bin/varneuron.sh $T $Smd $Mutr $Mutp $Xover &
                done
                wait $!
            done
        done
    done
done
echo
echo done!

#! /bin/sh

mkdir -p plots
mkdir -p tmp/plots
for B in $(seq 5 5 20); do
    for Rf in $(seq 0.0 0.25 1.0); do
        for Lf in $(seq 0.0 0.25 1.0); do
            echo bin/blotto.sh plots/blotto-$B-$Rf-$Lf $B $Rf $Lf
            bin/blotto.sh plots/blotto-$B-$Rf-$Lf $B $Rf $Lf &
        done
        wait $!
    done
done
echo
echo done!

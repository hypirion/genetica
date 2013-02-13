#! /bin/sh

ITERS=100

mkdir -p plots
for S in rank boltzmann sigma; do
    for C in full_replacement over_production generational_mixing; do
        if [ "$C" != "full_replacement" ]; then
            for I in $(seq 10 10 110); do
                echo bin/multirun.sh $ITERS plots/r-$S-$C-$I- $S $C $I
                bin/multirun.sh $ITERS plots/r-$S-$C-$I- $S $C $I &
            done
            wait $!
        else
            echo bin/multirun.sh $ITERS plots/r-$S-$C- $S $C 10
            bin/multirun.sh $ITERS plots/r-$S-$C- $S $C 10 &
        fi
    done
    wait $!
done
echo
echo done!


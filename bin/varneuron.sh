#! /bin/sh
echo izzy $1 $2 $3 $4 $5
bin/genetica.sh 200 100 tournament 60 0.2 rank generational_mixing 50 \
             spiking_neuron "train/izzy-train$1.dat" $2 $3 $4 $5 \
             tmp
OUT=$1-$2-$5-$3-$4
IN=tmp/$1-$2-$5-$3-$4
gnuplot -e \
  "outfile='plots/train-$OUT.png'; infile1='$IN-train'; infile2='train/izzy-train$1-gp.dat';" \
    bin/neuron.gp

gnuplot -e \
    "outfile='plots/fit-$OUT.png'; infile='$IN-fitness';" \
    bin/neuron-fit.gp
# generations popcount sel_method K eval_method protocol M module module_args
# sel_method -> tournament or roulette
# K -> tournament size
# P -> probability of picking random player from tournament
# eval_method -> fitness, sigma, boltzmann and rank
# protocol -> full_replacement, over_production, generational_mixing
# M -> # of parents to keep for generational_mixing, or # of additional children
#      to create (sum of children made = popcount + M)
# module -> what module to pick
# module_args -> variadic amount of module arguments

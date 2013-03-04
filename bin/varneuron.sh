#! /bin/sh
echo izzy $3 $4 $5 $6 $7
bin/genetica.sh 200 100 tournament 60 0.2 rank generational_mixing 50 \
             spiking_neuron "train/izzy-train$3.dat" $4 $5 $6 $7 \
             > $2
gnuplot -e \
  "outfile='$1.png'; infile1='$2'; infile2='train/izzy-train$3-gp.dat';" \
    bin/neuron.gp
  
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

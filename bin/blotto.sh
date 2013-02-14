#! /bin/sh
make --quiet compile && \
bin/genetica.sh 1500 20 roulette 30 0.2 sigma\
             generational_mixing 5 blotto $2 $3 $4 0.2 0.01 0.6 \
             | nl -s " " -b a > tmp/$1.dat && \
gnuplot -e "outfile='$1.png'; infile='tmp/$1.dat';" bin/blotto.gp
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

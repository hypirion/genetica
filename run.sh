#! /bin/sh
make --quiet compile && \
erl -noshell -s main start 100 70 roulette 10 0.2 sigma\
             generational_mixing 10 one_max 40 0.5 0.01\
             -s init stop | nl -s " " -b a > data.dat && \
gnuplot plotting.gnu && \
rm -f data.dat
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

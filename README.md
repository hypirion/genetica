# genetica

Evolutionary Algorithm(s) in Erlang

## How to add in new problems to solve

Genetica has a framework which allows one to create new modules and let them be
used in the main framework. This is done by following a "protocol", which
requires following functions to be available: `parse_args`, a function taking
the arguments given to this module and converts it into "proper" arguments.
`random_genotype_fn` takes parsed argument as input and returns a function
generating random genotypes. `phenotype_to_genotype_fn` and
`genotype_to_phenotype_fn` converts a phenotype to a genotype. If the conversion
from genotype to phenotype is not inversible, a common solution would be to let
the phenotype contain the genotype itself. `fitness_fn` returns a function which
evaluates the fitness of a phenotype, compared to the other phenotypes in the
population. `crossover_fn` returns a function which crosses two parent genotypes
and returns two children. `mutation_fn` returns a mutated genotype.

## License
Copyright © 2013 Jean Niklas L'orange

Distributed under the Eclipse Public License, the same as Clojure.

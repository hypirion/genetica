# genetica

Evolutionary Algorithm(s) in Erlang

## How to add in new problems to solve

Genetica has a framework which allows one to create new modules and let them be
used in the main framework. This is done by following a protocol, which requires
following functions to be available: `parse_args`, a function taking the
arguments given to this module and converts it into "proper" arguments.
`random_genotype_fn` takes parsed argument as input and returns a function
generating random genotypes. `genotype_to_phenotype_fn` takes parsed arguments
as input and returns a function which converts a genotype to a phenotype (A
process). `mutation_fn` takes the input arguments as input and returns a mutated
genotype. `analyze_fn` takes the input arguments as input, and returns a
function which takes a list of phenotypes coupled with their fitness, and
returns a list of numbers which may later be processed.

## Phenotypes as processes

`Phenotype ! {Pid, {gtype}}`

Sends a message to `Pid` on the form `{Phenotype, gtype, Genotype}`, where
`Genotype` is the genotype of the Phenotype.

`Phenotype ! {Pid, {fitness, PopulationPids}}`

Sends a message to `Pid` on the form `{Phenotype, fitness, Fitness}`, where
`Fitness` is the fitness of the Phenotype.

`Phenotype ! {Pid, {crossover, OtherPhenotype}}`

Performs a crossover of `Phenotype` and `OtherPhenotype`, and sends the genotype
of the crossover as a message on the form `{Phenotype, crossover, Genotype}` to
`Pid`.

`Phenotype ! {Pid, {shutdown}}`

Sends the message `{Phenotype, shutdown}` to `Pid` and shutdowns.

## License
Copyright © 2013 Jean Niklas L'orange

Distributed under the Eclipse Public License, the same as Clojure.

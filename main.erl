-module(main).
-export([start/1]).
-import(conversions, [bitstr_to_list/1, list_to_bitstr/1]).

start([Name]) ->
    random:seed(now()),
    Callme = fun() -> random:uniform(2) - 1 end,
    Bitstring = list_to_bitstr(repeatedly(20, Callme)),
    io:format("~w~nHello ~s!~n", [Bitstring, Name]).


%% (1) choose a genetic representation
%% (2) build a population
%% (3) design a fitness function
%% (4) choose a selection operator
%% (5) choose a recombination operator
%% (6) choose a mutation operator
%% (7) devise a data analysis procedure.

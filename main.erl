-module(main).
-export([start/1]).

start([Name]) ->
    io:format("~w~nHello ~s!~n",
              [bitstr_to_list(atom_to_binary(Name, latin1)), Name]).

bitstr_to_list(<<N:1,Rest/bitstring>>) ->
    [N | bitstr_to_list(Rest)];
bitstr_to_list(<<>>) ->
    [].

list_to_bitstr(List) ->
    list_to_bitstr(List, <<>>).

list_to_bitstr([], Bitstr) ->
    Bitstr;
list_to_bitstr([H | T], Bitstr) ->
    list_to_bitstr(T, <<Bitstr/bitstring, H:1>>).


%% (1) choose a genetic representation
%% (2) build a population
%% (3) design a fitness function
%% (4) choose a selection operator
%% (5) choose a recombination operator
%% (6) choose a mutation operator
%% (7) devise a data analysis procedure.

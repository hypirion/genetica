-module(conversions).
-export([bitstr_to_list/1, list_to_bitstr/1]).

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

-module(main).
-export([start/1]).

start([Name]) ->
    io:format("Hello ~s!~n", [Name]).

-module(selection_protocols).
-export([full_replacement_fn/1, over_production_fn/1,
         generational_mixing_fn/1]).

full_replacement_fn(_) ->
    fun (Children, _) ->
            Children
    end.

over_production_fn(_) ->
    fun (Children, _) ->
            Children
    end.

generational_mixing_fn(_) ->
    fun (Children, Parents) ->
            foo
    end.

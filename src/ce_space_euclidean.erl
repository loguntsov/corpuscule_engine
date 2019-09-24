-module(ce_space_euclidean).

-behavior(ce_space).

-export([
  distance/3,
  vector/3,
  sum/3,
  mul/3
]).

distance(Space, Coord1, Coord2) ->
  Delta = vector(Space, Coord1, Coord2),
  R = lists:foldl(fun(D, Acc) ->
    Acc + D * D
  end, 0, Delta),
  math:sqrt(R).

vector(_Space, Coord1, Coord2) ->
  lists:map(fun({A1, B1}) ->
    B1 - A1
  end, lists:zip(Coord1, Coord2)).

sum(_Space, Coord1, Coord2) ->
  lists:map(fun({A1, B1}) ->
    B1 + A1
  end, lists:zip(Coord1, Coord2)).

mul(_Space, Coord1, Multiplicator) when is_number(Multiplicator) ->
  lists:map(fun(X) ->
    X * Multiplicator
  end, Coord1).




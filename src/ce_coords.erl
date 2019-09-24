-module(ce_coords).

%% API
-export([
  new/1,
  zero/1,
  random/1,
  scale_by_world/2
]).

-type coords() :: [number()].
-export_type([
  coords/0
]).

new(List) when is_list(List)  ->
  lists:foreach(fun(Coord) ->
    case is_number(Coord) of
      true -> ok;
      false -> error(badarg, [List])
    end
  end, List),
  List.

zero(Dimension) ->
  lists:duplicate(Dimension, 0).

scale_by_world(Coords, World) ->
  T = ce_world:time_multiplicator(World),
  lists:map(fun(X) ->
    X*T
  end, Coords).

random(Dimension) ->
  lists:map(fun(_) ->
    % 10000*random:uniform()-5000
    % 100*random:uniform()-50
    100*(random:uniform()-0.5)
  end, zero(Dimension)).


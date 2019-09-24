-module(ce_example).

%% API
-export([
  start/0
]).

-include("corpuscule.hrl").

start() ->
  random:seed(os:timestamp()),
  ElectricIteraction = ce_interaction:new(ce_interaction_electric),
  World0 = ce_world:new(3, [ ElectricIteraction ]),
  { Points, Links } = ce_chains:chains(World0, [
    [ a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a3, a8, a1 ],
    [ a2, a4, a5, a7]
  ]),
  logger:info("Links ~p", [ Links ]),
  World1 = lists:foldl(fun(C, W) ->
    ce_world:add_corpuscule(W, C)
  end, World0, Points),
  World2 = lists:foldl(fun({{Id1, Id2}, D}, W) ->
    Link = ce_link_const_distance:new(World1, D),
    ce_world:add_link(W, Id1, Id2, Link)
  end, World1, Links),
  World3 = ce_world:set_time_multiplicator(World2, 0.1),
  Window = ce_canvas:start(),
  loop(Window, World3).

loop(Window, World) ->
  {World1, Changes } = ce_world:next_iteration(World),
  Max = maps:fold(fun( _, { List, _ }, M) ->
    lists:max([ M | [ abs(X) || X<- List ]])
  end, 0, Changes),
  World2 = ce_world:normalize(World1),
  ce_canvas:draw(Window, World2),
  timer:sleep(1),
  erlang:garbage_collect(),
  % logger:info("World ~p", [ World1 ]),
  loop(Window, World2).


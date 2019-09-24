-module(ce_example).

%% API
-export([
  start/0
]).

-include("corpuscule.hrl").

start() ->
  random:seed(os:timestamp()),
  ElectricIteraction = ce_interaction:new(ce_interaction_electric),
  World0 = ce_world:new(2, [ ElectricIteraction ]),
  { Points, Links } = ce_chains:chains(World0, [
      [ a1, a2, a3, a4, a5, a6, a1 ],
      [ a5, a6, a7, a11, a10, a1 ],
      [ b1, b2, b3, b1, a11, b3, a4 ]
  ]),
  logger:info("Links ~p", [ Links ]),
  World1 = lists:foldl(fun(C, W) ->
    ce_world:add_corpuscule(W, C)
  end, World0, Points),
  World2 = lists:foldl(fun({{Id1, Id2}, D}, W) ->
    Link = ce_link_const_distance:new(World1, D),
    ce_world:add_link(W, Id1, Id2, Link)
  end, World1, Links),
  World3 = ce_world:set_time_multiplicator(World2, 0.01),
  Window = ce_canvas:start(),
  loop(0, Window, World3).

loop(Count, Window, World) ->
  {World1, Changes } = ce_world:next_iteration(World),
  Max = maps:fold(fun( _, { List, _ }, M) ->
    lists:max([ M | [ abs(X) || X<- List ]])
  end, 0, Changes),

  DTime = if
    % Max > 100 -> ce_world:time_multiplicator(World1)/Max;
    Max > 5 -> ce_world:time_multiplicator(World1)/Max;
    Max < 0.5 -> ce_world:time_multiplicator(World1)*1.05;
    true -> ce_world:time_multiplicator(World1)
  end,
  World2 = ce_world:set_time_multiplicator(World1, DTime),
  World3 = ce_world:normalize(World2),
  ce_canvas:draw(Window, World3),
  timer:sleep(1),
  logger:info("Count ~p Time ~p Max=~p DTIme ~p ~p ", [ Count, ce_world:time(World3), Max, DTime, Max/ DTime ]),
  erlang:garbage_collect(),
  % logger:info("World ~p", [ World1 ]),
  case Max / DTime < 0.01 andalso ce_world:time(World3) > 1 of
    false -> loop(Count+1, Window, World3);
    true ->
      logger:info("Calculation is finished"),
      ok
  end.



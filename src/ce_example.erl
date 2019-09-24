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
  World3 = ce_world:set_time_multiplicator(World2, 0.1),
  Window = ce_canvas:start(),
  loop(0, Window, World3).

loop(Count, Window, World) ->
  Space = ce_world:space(World),
  Size = ce_world:size(World),
  Zero = ce_space:zero_coords(Space),
  {World1, Changes } = ce_world:next_iteration(World),
  {Max, S } = maps:fold(fun( _, { List, _ }, {M, Sum}) ->
    NewSum = ce_space:sum(Space, Sum, List),
    { lists:max([ M | [ abs(X) || X<- List ]]), NewSum }
  end, { 0, Zero }, Changes),
  Avg = ce_space:mul(Space, S, -1/Size),

  DisperseSum = maps:fold(fun(_, { List, _}, DSum) ->
    A = ce_space:sum(Space, List, Avg),
    ce_space:sum(Space, [ X*X || X<- A ], DSum)
  end, Zero, Changes),

  Disperse = ce_space:distance(Space, ce_space:mul(Space, DisperseSum, 1/Size), Zero),

  DTimeOld = ce_world:time_multiplicator(World1),
  DTime = if
    % Max > 100 -> ce_world:time_multiplicator(World1)/Max;
    Max > 7 -> DTimeOld/Max;
    Max < 0.5 -> DTimeOld*1.2;
    true -> DTimeOld
  end,
  World2 = ce_world:set_time_multiplicator(World1, DTime),
  World3 = ce_world:normalize(World2),
  ce_canvas:draw(Window, World3),
  logger:info("Count ~p Time ~p Max=~p Disperse= ~p DTIme ~p ~p ", [ Count, ce_world:time(World3), Max, Disperse, DTimeOld, Max/ DTimeOld ]),
  erlang:garbage_collect(),
  % logger:info("World ~p", [ World1 ]),
  case Max / DTimeOld< 0.01 andalso ce_world:time(World3) > 1 of
    false -> loop(Count+1, Window, World3);
    true ->
      logger:info("Calculation is finished"),
      ok
  end.



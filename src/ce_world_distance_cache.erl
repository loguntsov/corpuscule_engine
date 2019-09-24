-module(ce_world_distance_cache).

-include("corpuscule.hrl").

%% API
-export([
  new/0
]).

new() ->
  gb_trees:empty().

%%distance(Cache, Corpuscule1, Corpuscule2) ->
%%  { Flag, Key } = case {ce_corpuscule:id(Corpuscule1), ce_corpuscule:id(Corpuscule2)} of
%%    { A, B } when A>B -> { calc, {B,A } };
%%    { A, A } -> { none, undefined };
%%    K -> { calc, K }
%%  end,
%%  case Flag of
%%    none -> { 0, Cache};
%%    calc ->
%%      Space = ce_world:space(Space),
%%      Distance = ce_space:distance(Space, Corpuscule1, Corpuscule2),
%%      NewWorld =
%%  end,


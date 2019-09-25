-module(ce_world).

-compile({no_autoimport,[ size/1 ]}).

%% API
-export([
  new/1, new/2,
  space/1, time/1,
  size/1,
  scale_value/2, scale/2,
  optimize/1, normalize/1,
  next_iteration/1,
  find_corpuscule/2, get_corpuscule/2, all_corpuscules/1,
  update_corpuscule/2, update_corpuscules/2, add_corpuscule/2, remove_corpuscule/2,
  time_multiplicator/1, set_time_multiplicator/2,
  add_link/4,
  fold_by_corpuscules/3, fold_by_links/3
]).

-include("world.hrl").
-include("corpuscule.hrl").
-include("space.hrl").

-type world() :: #world{}.

-export_type([
  world/0
]).

new(Dimension) when is_integer(Dimension) ->
  new(Dimension, []).

new(Dimension, Interactions) when is_integer(Dimension) ->
  new(ce_space:new(ce_space_euclidean, Dimension), Interactions);

new(Space, Interactions) ->
  #world{
    space = Space,
    interactions = Interactions
  }.

space(World) ->
  World#world.space.

time(World) ->
  World#world.time.

optimize(World) -> World.

find_corpuscule(World, Id) ->
  case maps:find(Id, World#world.corpuscules) of
    { ok, Corpuscule} -> { ok, Corpuscule };
    error -> undefined
  end.

get_corpuscule(World, Id) ->
  case maps:find(Id, World#world.corpuscules) of
    { ok, C } -> C;
    undefined -> error({badarg, Id}, [ World ])
  end.

all_corpuscules(World) ->
  maps:values(World#world.corpuscules).

update_corpuscule(World, Corpuscule) ->
  World#world{
    corpuscules = maps:update(ce_corpuscule:id(Corpuscule), Corpuscule, World#world.corpuscules)
  }.

update_corpuscules(World, Corpuscules) ->
  NewMap = maps:from_list([ {ce_corpuscule:id(Corpuscule), Corpuscule} || Corpuscule <- Corpuscules ]),
  World#world{
    corpuscules = maps:merge(World#world.corpuscules, NewMap)
  }.

add_corpuscule(World, Corpuscule) ->
  World#world{
    corpuscules = maps:put(ce_corpuscule:id(Corpuscule), Corpuscule, World#world.corpuscules)
  }.

remove_corpuscule(World, Corpuscule = #corpuscule{}) ->
  remove_corpuscule(World, ce_corpuscule:id(Corpuscule));

remove_corpuscule(World, Id) ->
  World#world{
    corpuscules = maps:remove(Id, World#world.corpuscules)
  }.

set_time_multiplicator(World, TimeMultiplicator) ->
  World#world{
    time_multiplicator = TimeMultiplicator
  }.

time_multiplicator(World) ->
  World#world.time_multiplicator.

add_link(World, CId1, CId2, Link) ->
  C0_1 = get_corpuscule(World, CId1),
  C0_2 = get_corpuscule(World, CId2),
  C1_1 = ce_corpuscule:add_link(C0_1, CId2, Link),
  C1_2 = ce_corpuscule:add_link(C0_2, CId1, Link),
  update_corpuscules(World, [ C1_1, C1_2 ]).

scale_value(World, Value) ->
  Value * World#world.time_multiplicator.

scale(World, ScaleValue) ->
  Space = space(World),
  NewCorpuscules = maps:map(fun(Id, Corpuscule) ->
    Corpuscule#corpuscule{
      coords = ce_space:mul(Space, Corpuscule#corpuscule.coords, ScaleValue)
    }
  end, World#world.corpuscules),
  World#world{
    corpuscules = NewCorpuscules
  }.


fold_by_corpuscules(World, Acc, Fun) ->
  maps:fold(Fun, Acc, World#world.corpuscules).

fold_by_links(World, Acc, Fun) ->
  fold_by_corpuscules(World, Acc, fun(Id, Corpuscule, Ac) ->
    lists:foldl(fun({IdOther, Link}, A) ->
      Fun(Id, Corpuscule, IdOther, Link, A)
    end, Ac, ce_corpuscule:links(Corpuscule))
  end).

next_iteration(World) ->
  Space = space(World),
  PrepareInteraction = lists:map(fun(Interaction) ->
    PreparedData = ce_interaction:prepare(World, Interaction),
    { Interaction, PreparedData }
  end, World#world.interactions),
  Zero = ce_space:zero_coords(Space),

  InitChanges = maps:map(fun(_Id, _) ->
    {Zero, #{}}
  end, World#world.corpuscules),

  Corpuscules = ce_world:all_corpuscules(World),
  {ChangesMap, _  } = lists:foldl(fun(CorpusculeMain, { Changes, Cache }) ->
    Id = ce_corpuscule:id(CorpusculeMain),
    { Changes0, Cache0 } = lists:foldl(fun({ Interaction, PreparedData }, Acc) ->
      merge(Space, Acc, ce_interaction:calculate_static(Interaction, World, PreparedData, CorpusculeMain))
    end, {{ Zero, #{}}, Cache }, PrepareInteraction),

    { Changes1, Cache1 } = lists:foldl(fun
      (#corpuscule{ id = Id0 }, Acc) when Id0 =:= Id -> Acc;
      (Corpuscule, Acc0) ->
        { _, _} = lists:foldl(fun({ Interaction, PreparedData }, Acc) ->
          merge(Space, Acc, ce_interaction:calculate_dynamic(Interaction, World, PreparedData, CorpusculeMain, Corpuscule))
        end, Acc0, PrepareInteraction)
    end, { Changes0, Cache0 }, Corpuscules),

    { Changes2, Cache2 } = lists:foldl(fun({ IdSecondSide, Link }, Acc) ->
        COther = get_corpuscule(World, IdSecondSide),
        merge(Space, Acc, ce_link:calculate(World, Link, CorpusculeMain, COther))
    end, { Changes1, Cache1 }, CorpusculeMain#corpuscule.links),

    { maps:put(Id, Changes2, Changes), Cache2 }
  end, { InitChanges, _Cache = #{} }, Corpuscules),

  ChangesMap1 = maps:map(fun(_, { DeltaCoords, Opts }) ->
    { ce_coords:scale_by_world(DeltaCoords, World), Opts }
  end, ChangesMap),

  NewCorpusculs = maps:map(fun(Id, OldCorpuscule) ->
    { DeltaCoords, NewOpts } = maps:get(Id, ChangesMap1),
    OldCorpuscule#corpuscule{
      coords = ce_space:sum(Space, ce_corpuscule:coords(OldCorpuscule), DeltaCoords),
      options = maps:merge(ce_corpuscule:options(OldCorpuscule), NewOpts)
    }
  end, World#world.corpuscules),

  NewWorld = World#world{
    corpuscules = NewCorpusculs,
    time = time(World) + World#world.time_multiplicator
  },

  { NewWorld, ChangesMap1}.

size(World) ->
  maps:size(World#world.corpuscules).

normalize(World) ->
  Size = ?MODULE:size(World),
  Space = space(World),
  Sum = maps:fold(fun(_, C, Acc) ->
    ce_space:sum(Space, Acc, ce_corpuscule:coords(C))
  end, ce_space:zero_coords(Space), World#world.corpuscules),
  Avg = ce_space:mul(Space, Sum, -1/Size),
  NewCorpuscules = maps:map(fun(_Id, C) ->
    C#corpuscule{
      coords = ce_space:sum(Space, ce_corpuscule:coords(C), Avg)
    }
  end, World#world.corpuscules),
  World#world{
    corpuscules = NewCorpuscules
  }.

  %% INTERNAL

  merge(_Space, {{ _Coords, _Opts }, _Cache } = Acc, nothing) -> Acc;
  merge(Space, {{ Coords, Opts }, Cache }, { DeltaCoords, NewOpts }) ->
    {{ ce_space:sum(Space, Coords, DeltaCoords), maps:merge(Opts, NewOpts) }, Cache }.
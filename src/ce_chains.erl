-module(ce_chains).

%% API
-export([
  chains/2
]).

-include("corpuscule.hrl").

chains(World, ListOfLists) ->
  Dimension = ce_space:dimension(ce_world:space(World)),
  Uniq = lists:usort(lists:flatten(ListOfLists)),
  Points = lists:map(fun(Id) ->
    #corpuscule{
      id = Id,
      coords = ce_coords:random(Dimension),
      options = #{
        electric_q => 1
      }
    }
  end, Uniq),
  Links = lists:ukeysort(1, lists:flatmap(fun(List) ->
    lists:map(fun({Id1, Id2}) ->
      { link_id(Id1, Id2), 10}
    end, edges(List))
  end, ListOfLists)),
  { Points, Links}.

edges(List) ->
  edges(List, []).

edges([], Acc) -> Acc;
edges([_], Acc) -> Acc;

edges([ A, A | List], Acc) ->
  edges([ A | List ], Acc);

edges([ El1 | List], Acc) ->
  [ El2 | _ ] = List,
  edges(List, [{ El1, El2 } | Acc ]).

link_id(Id1, Id2) when Id2 < Id1 -> { Id2, Id1 };
link_id(Id1, Id2) -> {Id1, Id2}.

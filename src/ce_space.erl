-module(ce_space).

%% API
-export([
  new/2,
  module/1,
  dimension/1,
  zero_coords/1,
  distance/3,
  vector/3,
  sum/3, mul/3
]).

-record(space, {
  module :: atom(),
  dimension :: pos_integer(),
  options :: #{}
}).

-include("world.hrl").

-type space() :: #space{}.

-callback distance(Space :: space(), Coord1 :: ce_coords:coords(), Coord2 :: ce_coords:coords()) -> number().
-callback vector(Space :: space(), Coord1 :: ce_coords:coords(), Coord2 :: ce_coords:coords()) -> ce_coords:coords().
-callback sum(Space :: space(), Coord1 :: ce_coords:coords(), Coord2 :: ce_coords:coords()) -> ce_coords:coords().
-callback mul(Space :: space(), Coord :: ce_coords:coords(), Multiplicator :: number() | ce_coords:coords()) -> ce_coords:coords().

new(Module, Dimension) ->
  new(Module, Dimension, #{}).

new(Module, Dimension, Options) ->
  #space{
    module = Module,
    dimension = Dimension,
    options = Options
  }.

module(Space = #space{}) ->
  Space#space.module.

dimension(Space) ->
  Space#space.dimension.

zero_coords(Space) ->
  ce_coords:zero(dimension(Space)).

distance(_Space, Coord1, Coord2) when Coord1 =:= Coord2 -> 0;

distance(Space, Coord1, Coord2 ) ->
  Module = module(Space),
  Module:distance(Space, Coord1, Coord2).

vector(Space, Coord1, Coord2) ->
  Module = module(Space),
  Module:vector(Space, Coord1, Coord2).

sum(Space, Coord1, Coord2) ->
  Module = module(Space),
  Module:sum(Space, Coord1, Coord2).

mul(Space, Coords, Multiplicator) ->
  Module = module(Space),
  Module:mul(Space, Coords, Multiplicator).


-module(ce_interaction).

%% API
-export([
  new/1, new/2,
  module/1
]).

-export([
  prepare/2,
  calculate_static/4,
  calculate_dynamic/5
]).

-include("interaction.hrl").

-type interaction() :: #interaction{}.
-type prepared_data() :: term().

-export_type([
  interaction/0
]).

-callback prepare(World :: ce_world:world(), Interaction :: interaction()) -> prepared_data().
-callback calculate_static(World :: ce_world:world(), prepared_data(), Corpuscule :: ce_corpuscule:corpuscule()) -> nothing | { DeltaCoords :: ce_coords:coords(), NewOptions :: maps:map() }.
-callback calculate_dynamic(World :: ce_world:world(), prepared_data(), CorpusculeMain :: ce_corpuscule:corpuscule(), Corpuscule :: ce_corpuscule:corpuscule()) -> nothing | { DeltaCoords :: ce_coords:coords(), NewOptions :: maps:map() }.

new(Module) ->
  new(Module, #{}).

new(Module, Options) ->
  #interaction{
    module = Module,
    options = Options
  }.

module(Interacton) ->
  Interacton#interaction.module.

prepare(World, Interaction) ->
  Module = module(Interaction),
  Module:prepare(World, Interaction).

calculate_static(Interaction, World, PreparedData, Corpuscule) ->
  Module = module(Interaction),
  Module:calculate_static(World, PreparedData, Corpuscule).

calculate_dynamic(Interaction, World, PreparedData, CorpusculeMain, Corpuscule) ->
  Module = module(Interaction),
  Module:calculate_dynamic(World, PreparedData, CorpusculeMain, Corpuscule).

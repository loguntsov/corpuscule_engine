-module(ce_link).

%% API
-export([
  new/3,
  args/1,
  calculate/4
]).

-record(link,{
  module :: atom(),
  args :: maps:map()
}).

-type link() :: #link{}.
-export_type([
  link/0
]).

-callback init(World :: ce_world:world(), Link :: link()) -> NewLink :: link().
-callback calculate(World :: ce_world:world(), Link :: link(), CMain :: ce_corpuscule:corpuscule(), COther :: ce_corpuscule:corpuscule() ) -> { DeltaCoords :: ce_coods:coods(), Opts :: maps:map() }.

-optional_callbacks([
  init/2
]).

new(World, Module, Args) ->
  Link = #link{
    module = Module,
    args = Args
  },
  case erlang:function_exported(Module, init, 2) of
    false -> Link;
    true ->
      Module:init(World, Link)
  end.

module(Link) ->
  Link#link.module.

calculate(World, Link, CMain, COther) ->
  Module = module(Link),
  Module:calculate(World, Link, CMain, COther).

args(Link) ->
  Link#link.args.
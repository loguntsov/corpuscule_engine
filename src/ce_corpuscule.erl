-module(ce_corpuscule).

%% API
-export([
  new/3,
  module/1, id/1, coords/1, options/1,
  links/1, add_link/3, remove_link/2, remove_link/3,
  opt/3
]).

-include("corpuscule.hrl").

-type id() :: term().
-type corpuscule() :: #corpuscule{}.
-export_type([
  id/0,
  corpuscule/0
]).

-callback init(Options :: maps:map() ) -> corpuscule().

new(Module, Id, Options) when is_atom(Module) ->
  Corp = #corpuscule{} = Module:init(Options),
  Corp#corpuscule{
    module = Module,
    id = Id
  }.

module(Corpuscule) ->
  Corpuscule#corpuscule.module.

id(Corpuscule) ->
  Corpuscule#corpuscule.id.

coords(Corpuscule) ->
  Corpuscule#corpuscule.coords.

options(Corpuscule) ->
  Corpuscule#corpuscule.options.

opt(Corpuscule, Key, Default) ->
  maps:get(Key, options(Corpuscule), Default).

add_link(Corpuscule, IdOther, Link) ->
  Corpuscule#corpuscule{
    links = [{ IdOther, Link} | Corpuscule#corpuscule.links ]
  }.

links(Corpuscule) ->
  Corpuscule#corpuscule.links.

remove_link(Corpuscule, IdOther) ->
  remove_link(Corpuscule, IdOther, fun(_Link) -> true end).

remove_link(Corpuscule, IdOther, Fun) ->
  NewLinks = lists:filter(fun({ Id, L }) ->
    Id =/= IdOther andalso not(Fun(L))
  end, Corpuscule#corpuscule.links),
  Corpuscule#corpuscule{
    links = NewLinks
  }.

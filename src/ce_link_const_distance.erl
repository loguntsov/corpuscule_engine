-module(ce_link_const_distance).

%% API
-export([
  new/2
]).

-behavior(ce_link).
-export([
  init/2,
  calculate/4
]).

new(World, Distance) ->
  ce_link:new(World, ?MODULE, Distance).

init(World, Link) -> Link.

calculate(World, Link, CorpusculeMain, CorpusculeOther ) ->
  DistanceLimit = ce_link:args(Link),
  Space = ce_world:space(World),
  CoordsMain = ce_corpuscule:coords(CorpusculeMain),
  CoordsOther = ce_corpuscule:coords(CorpusculeOther),
  Distance = ce_space:distance(Space, CoordsMain, CoordsOther),
  Vector = ce_space:vector(Space, CoordsOther, CoordsMain),
  D = (DistanceLimit-Distance),
  F = 0.0005*D*abs(D), %(0.5*(min(1, max(0, ce_world:time(World)-10)/10)))*D,
  { ce_space:mul(Space, Vector, F/Distance), #{} }.

%% INTERNAL

sign(A) when A < 0 -> -1;
sign(A) -> 1.
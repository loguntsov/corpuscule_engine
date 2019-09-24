-module(ce_interaction_electric).

%% API
-behavior(ce_interaction).

-export([
  prepare/2,
  calculate_static/3,
  calculate_dynamic/4
]).

prepare(_World, _Interaction) ->
  {}.

calculate_static(_World, _PreparedData = {}, _Corpuscule) -> nothing.
calculate_dynamic(World, _PreparedData = {}, CorpusculeMain, CorpusculeOther) ->
  QMain = ce_corpuscule:opt(CorpusculeMain, electric_q, 0),
  QOther = ce_corpuscule:opt(CorpusculeOther, electric_q, 0),
  if
    QMain =:= 0 -> nothing;
    QOther =:= 0 -> nothing;
    true ->
      Space = ce_world:space(World),
      CoordsMain = ce_corpuscule:coords(CorpusculeMain),
      CoordsOther = ce_corpuscule:coords(CorpusculeOther),
      Distance = ce_space:distance(Space, CoordsMain, CoordsOther),
      Vector = ce_space:vector(Space, CoordsMain, CoordsOther),
      F = -QMain*QOther/Distance*100,
      { ce_coords:scale_by_world(ce_space:mul(Space, Vector, F/Distance), World), #{} }
  end.

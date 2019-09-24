-module(ce_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

start(_StartType, _StartArgs) ->
  { ok, Pid } = ce_app_sup:start_link(),
  proc_lib:spawn_link(fun() ->
    ce_example:start()
  end),
  { ok, Pid }.

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

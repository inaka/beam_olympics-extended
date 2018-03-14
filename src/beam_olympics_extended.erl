-module(beam_olympics_extended).

-behaviour(application).

-export([start/2, stop/1]).

%%==============================================================================
%% API
%%==============================================================================
start(_Type, _Args) ->
  ok = pg2:create(websockets),
  start_cowboy().

stop(_State) ->
  ok.

%%==============================================================================
%% Utils
%%==============================================================================
start_cowboy() ->
  Dispatch = {dispatch, cowboy_router:compile(get_dispatch())},
  cowboy:start_http(http, 100, [{port, 8081}], [{env, [Dispatch]}]).

get_dispatch() ->
  Path = code:priv_dir(beam_olympics_extended) ++ "/web",
  [{'_', [{"/ws", ws_handler, []},
          {"/", cowboy_static, {file, Path ++ "/index.html"}},
          {"/main.js", cowboy_static, {file, Path ++ "/main.js"}},
          {"/style.css", cowboy_static, {file, Path ++ "/style.css"}}]}].

-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3]).
-export([websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%%==============================================================================
%% API
%%==============================================================================
init({_TransportName, _ProtocolName}, Req, Opts) ->
  {upgrade, protocol, cowboy_websocket, Req, Opts}.

websocket_init(_TransportName, Req, _Opts) ->
  ok = pg2:join(websockets, self()),
  % This ensures the initial stats are sent
  #{players := Players} = gen_server:call(bo_server, stats),
  ok = lists:foreach(fun(#{name := Name, done := Done, score := Score}) ->
                       Tasks = length(bo_tasks:all()),
                       self() ! {added, Name, Done, Tasks, Score}
                     end, Players),

  {ok, Req, {}}.

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({added, Name, CurrentTasks, TotalTasks, Score}, Req, State) ->
  Msg = #{ 'event'        => <<"added">>
         , 'name'         => Name
         , 'currentTasks' => CurrentTasks
         , 'totalTasks'   => TotalTasks
         , 'score'        => Score
         },
  {reply, {text, jiffy:encode(Msg)}, Req, State};
websocket_info({changed, Name, CurrentTasks, TotalTasks, Score}, Req, State) ->
  Msg = #{ 'event'        => <<"changed">>
         , 'name'         => Name
         , 'currentTasks' => CurrentTasks
         , 'totalTasks'   => TotalTasks
         , 'score'        => Score
         },
  {reply, {text, jiffy:encode(Msg)}, Req, State};
websocket_info(_Msg, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
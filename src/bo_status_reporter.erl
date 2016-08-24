-module(bo_status_reporter).

%% API
-export([start/0, tick/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec start() -> ok.
start() ->
  _ = erlcloud:start(),
  ok = schedule_tick(1500).

-spec tick() -> ok.
tick() ->
  Key = application:get_env(beam_olympics, s3_key),
  Secret = application:get_env(beam_olympics, s3_secret),
  Bucket = application:get_env(beam_olympics, s3_bucket),
  ok = erlcloud_ec2:configure(Key, Secret),
  _ = erlcloud_s3:put_object(Bucket, filename(), get_instructions()),
  ok.

%%==============================================================================
%% Utils
%%==============================================================================
get_instructions() ->
  Filename = "lib/beam_olympics-private-1.0.0/priv/" ++ filename(),
  {ok, Data} = file:read_file(Filename),
  replace(Data, [ {<<"{cookie}">>, atom_to_binary(erlang:get_cookie(), utf8)}
                , {<<"{node}">>, atom_to_binary(erlang:node(), utf8)}]).

replace(Bin, []) ->
  Bin;
replace(Bin, [H | T]) ->
  replace(replace(Bin, H), T);
replace(Bin, {Pattern, Replacement}) ->
  binary:replace(Bin, Pattern, Replacement).

schedule_tick(Time) ->
  {ok, _TRef} = timer:apply_after(Time, ?MODULE, tick, []),
  ok.

filename() ->
  "instructions.rtf".
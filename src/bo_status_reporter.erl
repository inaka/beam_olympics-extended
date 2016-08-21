-module(bo_status_reporter).

%% API
-export([start/0, tick/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec start() -> ok.
start() ->
  _ = erlcloud:start(),
  ok = schedule_tick(1600).

-spec tick() -> ok.
tick() ->
  Instructions = get_instructions(),
  ok = erlcloud_ec2:configure(key(), secret()),
  _ = erlcloud_s3:put_object(bucket(), filename(), Instructions),
  ok = schedule_tick().

%%==============================================================================
%% Utils
%%==============================================================================
get_instructions() ->
  Filename = "lib/beam_olympics-private-1.0.0/priv/instructions.rtf",
  {ok, Data} = file:read_file(Filename),
  replace(Data, {<<"{cookie}">>, atom_to_binary(erlang:get_cookie(), utf8)}).

replace(Bin, []) ->
  Bin;
replace(Bin, [H | T]) ->
  replace(replace(Bin, H), T);
replace(Bin, {Pattern, Replacement}) ->
  binary:replace(Bin, Pattern, Replacement).

schedule_tick() ->
  schedule_tick(60000).

schedule_tick(Time) ->
  {ok, _TRef} = timer:apply_after(Time, ?MODULE, tick, []),
  ok.

%%==============================================================================
%% Constants
%%==============================================================================
key() ->
  "AKIAIT5KCPYPSN3KEJHQ".
secret() ->
  "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".
bucket() ->
  "inaka-elixir-challenge".
filename() ->
  "instructions.rtf".
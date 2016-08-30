-module(bo_status_reporter).

%% API
-export([start/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec start() -> ok.
start() ->
  _ = erlcloud:start(),
  {ok, Key} = application:get_env(beam_olympics, s3_key),
  {ok, Secret} = application:get_env(beam_olympics, s3_secret),
  {ok, Bucket} = application:get_env(beam_olympics, s3_bucket),
  ok = erlcloud_ec2:configure(Key, Secret),
  ok = upload_instructions("rtf", Bucket),
  ok = upload_instructions("html", Bucket).

%%==============================================================================
%% Utils
%%==============================================================================
upload_instructions(Extension, Bucket) ->
  Filename = filename(Extension),
  Instructions = get_instructions(Extension),
  _ = erlcloud_s3:put_object(Bucket, Filename, Instructions),
  ok.

get_instructions(Extension) ->
  PrivDir = code:priv_dir(beam_olympics_private),
  Filename = filename:join(PrivDir, filename(Extension)),
  {ok, Data} = file:read_file(Filename),
  replace(Data, [ {<<"{cookie}">>, atom_to_binary(erlang:get_cookie(), utf8)}
                , {<<"{node}">>, atom_to_binary(erlang:node(), utf8)}]).

replace(Bin, []) ->
  Bin;
replace(Bin, [H | T]) ->
  replace(replace(Bin, H), T);
replace(Bin, {Pattern, Replacement}) ->
  binary:replace(Bin, Pattern, Replacement).

filename(Extension) ->
  "instructions." ++ Extension.
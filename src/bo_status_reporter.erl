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
  _ = erlcloud_s3:put_object(Bucket, filename(), get_instructions()),
  ok.

%%==============================================================================
%% Utils
%%==============================================================================
get_instructions() ->
  PrivDir = priv_dir(),
  Filename = filename:join(PrivDir, filename()),
  {ok, Data} = file:read_file(Filename),
  replace(Data, [ {<<"{cookie}">>, atom_to_binary(erlang:get_cookie(), utf8)}
                , {<<"{node}">>, atom_to_binary(erlang:node(), utf8)}]).

replace(Bin, []) ->
  Bin;
replace(Bin, [H | T]) ->
  replace(replace(Bin, H), T);
replace(Bin, {Pattern, Replacement}) ->
  binary:replace(Bin, Pattern, Replacement).

filename() ->
  "instructions.rtf".

priv_dir() ->
  case code:priv_dir('beam_olympics-private') of
    Path when is_list(Path) ->
      Path;
    _ ->
      "lib/beam_olympics-private-1.0.0/priv/"
  end.
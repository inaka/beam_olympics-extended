-module(bo_utils).

%% API
-export([difficulty/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec difficulty() -> hard | normal | easy.
difficulty() ->
  {ok, Difficulty} = application:get_env(beam_olympics, difficulty, normal),
  get_difficulty(Difficulty).

%%==============================================================================
%% Utils
%%==============================================================================
get_difficulty(Difficulty) when Difficulty =:= hard;
                                Difficulty =:= normal;
                                Difficulty =:= easy ->
  Difficulty;
get_difficulty(_Other) ->
  normal.
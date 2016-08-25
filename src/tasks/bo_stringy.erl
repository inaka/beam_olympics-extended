-module(bo_stringy).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Stringy: We all love working with strings on elixir (erlang"
                   " binaries), why not have some fun capitalising every other "
                   "WORD in a sentence? So \"a horse is a horse, of course, of "
                   "course\" becomes \"A horse Is a Horse, of Course, of "
                   "Course\"">>.

-spec spec() -> bo_task:spec().
spec() -> #{input => [<<"binary()">>], output => <<"binary()">>}.

-spec score() -> 150.
score() -> 150.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(phrase())].

build_test(Bin) ->
  fun(Fun) ->
    R = solve(Bin),
    try Fun(Bin) of
      R   -> ok;
      Bad -> {error, #{ input => Bin
                      , output => Bad
                      , expected => R
                      }}
    catch
      _:Error ->
        {error, #{ input => Bin
                 , output => Error
                 , stack => erlang:get_stacktrace()
                 , expected => <<"This isn't what \"Let it crash\" means">>
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
phrase() ->
  <<"Is this really that complicated? It should be trivial... Unless someone ",
    "went in and added a number, say 1234 in the middle of the phrase.">>.

solve(Bin) ->
  Words = [binary_to_list(B) || B <- binary:split(Bin, <<" ">>, [global])],
  {_, Result} = lists:foldl(fun(W, {true, Acc}) ->
                                case word(W) of
                                  true  -> {false, [up(W)| Acc]};
                                  false -> {true, [W | Acc]}
                                end;
                               (W, {false, Acc}) ->
                                case word(W) of
                                  true  -> {true,  [W | Acc]};
                                  false -> {false, [W | Acc]}
                                end
                            end, {true, []}, Words),
  list_to_binary(string:join(lists:reverse(Result), " ")).

word([Char | _]) ->
  (Char >= $a andalso Char =< $z) orelse (Char >= $A andalso Char =< $Z).

up([Char | Rest]) when Char >= $a, Char =< $z ->
  [Char + $A - $a | Rest];
up(Str) ->
  Str.
-module(bo_properly).

-behaviour(bo_task).

-export([description/0, spec/0, score/0, timeout/0, tests/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec description() -> binary().
description() -> <<"Do it properly: You will receive a list, make sure that "
                   "list and any nested lists are proper lists. Return that "
                   "list.">>.

-spec spec() -> bo_task:spec().
spec() -> #{ input => [<<"[boolean()]">>, <<"char | short | int">>]
           , output => <<"binary()">>
           }.

-spec score() -> 150.
score() -> 150.

-spec timeout() -> 5000.
timeout() -> 5000.

-spec tests() -> [bo_task:test()].
tests() -> [build_test(Case) || Case <- cases()].

build_test({Improper, Proper}) ->
  fun(Fun) ->
    try Fun(Improper) of
      Proper -> ok;
      Bad    -> {error, #{ input => Improper
                         , output => Bad
                         , expected => Proper
                         }}
    catch
      _:Error ->
        {error, #{ input => Improper
                 , output => Error
                 , stack => erlang:get_stacktrace()
                 , expected => <<"It should not crash">>
                 }}
    end
  end.

%%==============================================================================
%% Utils
%%==============================================================================
cases() ->
  [ { []
    , []
    }
  , { [a, b, c, [d, e, f]]
    , [a, b, c, [d, e, f]]
    }
  , { [a, b, c, d | e]
    , [a, b, c, d, e]
    }
  , { [a, b, c, [1, 2, 3, 4] | g]
    , [a, b, c, [1, 2, 3, 4], g]
    }
  , { [a, b, c, [1, 2, 3, 4 | 5] | d]
    , [a, b, c, [1, 2, 3, 4, 5], d]
    }
  , { [a, b, c, [1, [true | false], 3, 4 | 5] | d]
    , [a, b, c, [1, [true, false], 3, 4, 5], d]
    }
  , { [a, [b, c, [d, e, [f, g] | h], i, j] | k]
    , [a, [b, c, [d, e, [f, g], h], i, j], k]
    }
  ].
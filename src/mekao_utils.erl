-module(mekao_utils).

%% API
-export([
    identity/1,
    intersperse/2, intersperse/3,
    intersperse2/4,

    map2/3, map3/4
]).

-include("mekao.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec map2( fun( ( V1 :: term(), V2 :: term()) -> ResV :: term() )
          , L1 :: list(), L2 :: list()) -> list().

map2(_RetFun, [], []) ->
    [];

map2(RetFun, [V1 | L1], [V2 | L2]) ->
    [RetFun(V1, V2) | map2(RetFun, L1, L2)].


-spec map3( fun( ( V1 :: term(), V2 :: term(), V3 :: term()) -> ResV :: term() )
          , L1 :: list(), L2 :: list(), L3 :: list()) -> list().

map3(_RetFun, [], [], []) ->
    [];

map3(RetFun, [V1 | L1], [V2 | L2], [V3 | L3]) ->
    [RetFun(V1, V2, V3) | map3(RetFun, L1, L2, L3)].


-spec identity(term()) -> term().
identity(X) -> X.

intersperse(List, Sep) ->
    intersperse(List, Sep, fun identity/1).

-spec intersperse( List :: list()
                 , Separator :: term()
                 , ValFun :: fun((term()) -> term())
                 ) -> list().
intersperse([], _, _) ->
    [];
intersperse([Item], _, ValFun) ->
    [ValFun(Item)];
intersperse([Item | Items], Sep, ValFun) ->
    [ValFun(Item), Sep | intersperse(Items, Sep, ValFun)].


intersperse2(_ValFun, _Sep, [], []) ->
    [];
intersperse2(ValFun, _Sep, [I1], [I2]) ->
    [ValFun(I1, I2)];
intersperse2(ValFun, Sep, [I1 | I1s], [I2 | I2s]) ->
    [ValFun(I1, I2), Sep | intersperse2(ValFun, Sep, I1s, I2s)].


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

map2_test_() ->
    F = fun (A, B) -> A + B end,
    [
        ?_assertMatch([], map2(F, [], [])),
        ?_assertMatch([3], map2(F, [1], [2])),
        ?_assertMatch([10, 10, 10], map2(F, [1, 2, 3], [9, 8, 7])),
        ?_assertException(error, function_clause, map2(F, [1], [])),
        ?_assertException(error, function_clause, map2(F, [], [1]))
    ].

map3_test_() ->
    F = fun (A, B, C) -> A + B + C end,
    [
        ?_assertMatch([], map3(F, [], [], [])),
        ?_assertMatch([6], map3(F, [1], [2], [3])),
        ?_assertMatch([15, 15, 15], map3(F, [1, 2, 3], [9, 8, 7], [5, 5, 5])),
        ?_assertException(error, function_clause, map3(F, [1], [], [])),
        ?_assertException(error, function_clause, map3(F, [], [1], [])),
        ?_assertException(error, function_clause, map3(F, [], [], [1]))
    ].

intersperse_test_() ->
    L = [a, b, c],
    ValFun = fun (a) -> 1; (b) -> 2; (c) -> 3 end,

    [
        ?_assert([1, x, 2, x, 3] == intersperse(L, x, ValFun)),
        ?_assert([1] == intersperse([a], x, ValFun)),
        ?_assert([] == intersperse([], x, ValFun))
    ].

intersperse2_test_() ->
    ValFun = fun (V1, V2) -> {V1, V2} end,

    [
        ?_assert(
           intersperse2(ValFun, x, [a, b, c, d], [d, c, b, a])
           == [{a, d}, x, {b, c}, x, {c, b}, x, {d, a}]
        ),
        ?_assert(intersperse2(ValFun, x, [a], [b]) == [{a, b}]),
        ?_assert(intersperse2(ValFun, x, [], []) == [])
    ].

-endif.

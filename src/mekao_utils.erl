-module(mekao_utils).

%% API
-export([
    identity/1,
    intersperse/2, intersperse/3,
    intersperse4/3,

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


intersperse4({[], [], [], []}, _, _) ->
    [];
intersperse4({[I1], [I2], [I3], [I4]}, _, ValFun) ->
    [ValFun({I1, I2, I3, I4})];
intersperse4({[I1 | I1s], [I2 | I2s], [I3 | I3s], [I4 | I4s]}, Sep, ValFun) ->
    [ValFun({I1, I2, I3, I4}), Sep | intersperse4({I1s, I2s, I3s, I4s}, Sep, ValFun)].


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

intersperse4_test_() ->
    L4 = {[a, b, c, d], [b, c, d, a], [c, d, a, b], [d, a, b, c]},

    ValFun =
        fun
            ({a, _, _, _}) -> 1;
            ({b, _, _, _}) -> 2;
            ({c, _, _, _}) -> 3;
            ({d, _, _, _}) -> 4
        end,

    [
        ?_assert([1, x, 2, x, 3, x, 4] == intersperse4(L4, x, ValFun)),
        ?_assert([1] == intersperse4({[a], [b], [c], [d]}, x, ValFun)),
        ?_assert([] == intersperse4({[], [], [], []}, x, ValFun))
    ].

-endif.

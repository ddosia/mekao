-module(mekao_utils).

%% API
-export([
    identity/1,
    intersperse/2, intersperse/3,
    intersperse4/3
]).


%% ===================================================================
%% API functions
%% ===================================================================

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

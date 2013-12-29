-module(mekao_utils).

%% API
-export([
    identity/1,
    intersperse/2, intersperse/3,
    intersperse4/3,

    e_diff/3, e_diff/4
]).

-include("mekao.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec e_diff( E1 :: tuple() | list()
            , E2 :: tuple() | list()
            , mekao:table() | [mekao:column()]
            ) -> list().
%% @doc compares not primary key columns, and if they equals
%%      changes val to '$skip'.
e_diff(E1, E2, Table) ->
    RetFun =
        fun
            (V,  V,  #mekao_column{key = true}) -> V;
            (V,  V,  #mekao_column{key = false}) -> '$skip';
            (V1, V2, #mekao_column{key = false}) when V1 /= V2 -> V2
        end,
    e_diff(E1, E2, Table, RetFun).


-spec e_diff( E1 :: tuple() | list()
            , E2 :: tuple() | list()
            , mekao:table() | [mekao:column()]
            , fun( ( Val1 :: term()
                   , Val2 :: term()
                   , mekao:column()
                   ) -> NewVal :: term())
            ) -> list().
e_diff(E1, E2, Table, RetFun) when is_tuple(E1) ->
    [_EntityName | E1Vals] = tuple_to_list(E1),
    e_diff(E1Vals, E2, Table, RetFun);

e_diff(E1, E2, Table, RetFun) when is_tuple(E2) ->
    [_EntityName | E2Vals] = tuple_to_list(E2),
    e_diff(E1, E2Vals, Table, RetFun);

e_diff(E1, E2, #mekao_table{columns = Cols}, RetFun) ->
    e_diff(E1, E2, Cols, RetFun);

e_diff([], [], [], _RetFun) ->
    [];

e_diff([V1 | E1], [V2 | E2], [C | Cols], RetFun) ->
    [RetFun(V1, V2, C) | e_diff(E1, E2, Cols, RetFun)].


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

e_diff_test_() ->
    E1 = {book, 1, Author = <<"Lev Tolstoy">>, {{1869, 1, 1}, {0, 0, 0}}},
    E2 = {book, 1, Author, D2 = {{1869, 1, 2}, {3, 4, 5}}},

    Table = #mekao_table{
        name = <<"books">>,
        columns = [
            #mekao_column{name = <<"id">>, type = int, key = true},
            #mekao_column{name = <<"author">>, type = varchar},
            #mekao_column{name = <<"created">>, type = datetime}
        ]
    },
    [
        ?_assertMatch(
            [1, '$skip', D2], e_diff(E1, E2, Table)
        ),
        ?_assertMatch(
            ['$skip', '$skip', '$skip'],
            e_diff(E1, E2, Table, fun(_, _, _) -> '$skip' end)
        )
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

-module(mekao).

%% API
-export([
    insert/3
]).

-include("mekao.hrl").

-type entity()  :: tuple().

-type table()   :: #mekao_table{}.
-type field()   :: #mekao_field{}.
-type type()    :: term().
-type value()   :: term().

-type 'query'() :: {SQLText :: iolist(), Fields :: [iolist()], Types :: [term()]}.

-opaque ftvs()  :: {Fields :: [field()], Types :: [type()], Values :: [value()]}.
-opaque s()     :: #mekao_settings{}.

-export_type([
    table/0,
    field/0,
    type/0
]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec insert(entity(), table(), s()) -> 'query'().
insert(E, Table, S) when is_tuple(E) ->
    do_insert(tuple_to_list(E), Table, S).

do_insert([_EName | AllVals], Table, S) ->
    {Fields, Types, Vals} = ftvs(AllVals, Table),
    Q = [
        "INSERT INTO ", Fields,
        " VALUES (", placeholders(length(Fields), S), ") ",
        return(Table, S)
    ],
    {Q, Types, Vals}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ftvs([value()], table() | [field()]) -> ftvs().
ftvs(Vs, #mekao_table{fields = Fs}) ->
    ftvs(Vs, Fs);
ftvs([], []) ->
    {[], [], []};
ftvs([_ | Vs], [#mekao_field{ro = RO, key = Key} | Fs]) when RO; Key ->
    ftvs(Vs, Fs);
ftvs([V | Vs], [#mekao_field{type = T, name = F} | Fs]) ->
    {ResFs, ResTs, ResVs} = ftvs(Vs, Fs),
    {[F | ResFs], [T | ResTs], [V | ResVs]}.


-spec placeholders(non_neg_integer(), s()) -> iolist().
placeholders(0, _) ->
    [];
placeholders(C, S = #mekao_settings{ph_fun = PHFun}) when is_integer(C), C > 0 ->
    [PHFun(C) | placeholders(C - 1, S)].


-spec return(table(), s()) -> iolist().
return(Table, #mekao_settings{ret_fun = RetFun}) ->
    RetFun(Table).

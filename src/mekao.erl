-module(mekao).

%% API
-export([
    insert/2
]).

-include("mekao.hrl").

-type entity()  :: tuple().

-type table()   :: #table{fields :: [field()]}.
-type field()   :: #field{type :: type()}.
-type type()    :: term().
-type value()   :: term().

-type 'query'() :: {SQLText :: iolist(), Fields :: [iolist()], Types :: [term()]}.

-opaque ftvs()  :: {Fields :: [field()], Types :: [type()], Values :: [value()]}.
-opaque s()     :: #s{}.

%% ===================================================================
%% API functions
%% ===================================================================

-spec insert(entity(), s()) -> 'query'().
insert(E, S) when is_tuple(E) ->
    do_insert(tuple_to_list(E), S).

do_insert([EName | AllVals], S) ->
    Table = table(EName, S),
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

-spec table(entity(), s()) -> table().
table(EName, #s{tables = Tables}) ->
    orddict:fetch(EName, Tables).


-spec ftvs([value()], table()) -> ftvs().
ftvs(Vs, #table{fields = Fs}) ->
    ftvs(Vs, Fs);
ftvs([], []) ->
    {[], [], []};
ftvs([_ | Vs], [#field{ro = RO, key = Key} | Fs]) when RO; Key ->
    ftvs(Vs, Fs);
ftvs([V | Vs], [#field{type = T, name = F} | Fs]) ->
    {ResFs, ResTs, ResVs} = ftvs(Vs, Fs),
    {[F | ResFs], [T | ResTs], [V | ResVs]}.


-spec placeholders(non_neg_integer(), s()) -> iolist().
placeholders(0, _) ->
    [];
placeholders(C, #s{ph_fun = PHFun}) when is_integer(C), C > 0 ->
    [PHFun(C) | placeholders(C - 1, PHFun)].


-spec return(table(), s()) -> iolist().
return(Table, #s{ret_fun = RetFun}) ->
    RetFun(Table).

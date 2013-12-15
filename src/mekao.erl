-module(mekao).

%% API
-export([
    select/3, select_pk/3,
    insert/3,
    update/4, update_pk/3
]).

-include("mekao.hrl").

-type entity()  :: tuple().

-type table()   :: #mekao_table{}.
-type field()   :: #mekao_field{}.
-type type()    :: term().
-type value()   :: '$skip' | term().

-type 'query'() :: {SQLText :: iolist(), Fields :: [iolist()], Types :: [term()]}.

-opaque fphtvs()  :: { Fields       :: [iolist()]
                     , PlaceHolders :: [iolist()]
                     , Types        :: [type()]
                     , Values       :: [value()]
                     }.
-opaque s()     :: #mekao_settings{}.

-export_type([
    table/0,
    field/0,
    type/0
]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec select_pk(entity(), table(), s()) -> 'query'().
%% @doc Reads entity by primary key
select_pk(E, Table, S) when is_tuple(E) ->
    SkipFun = fun(#mekao_field{key = Key}) -> not Key end,
    do_select(SkipFun, tuple_to_list(E), Table, S).


-spec select(entity(), table(), s()) -> 'query'().
%% @doc Reads entities, skips fields from `WHERE' clause with `$skip' value
select(E, Table, S) when is_tuple(E) ->
    SkipFun = fun (_) -> false end,
    do_select(SkipFun, tuple_to_list(E), Table, S).

do_select(SkipFun, [_EName | AllVals], Table, S) ->
    QData = {_, _, Types, Vals} = unpack(SkipFun, AllVals, Table, S),
    Q = [
        "SELECT ",
            fields(Table),
        " FROM ",
            table(Table),
        " WHERE ",
            join_conditions(QData, " AND ")
    ],
    {Q, Types, Vals}.


-spec insert(entity(), table(), s()) -> 'query'().
%% @doc Insert entity, omits fields with `$skip' value.
insert(E, Table, S) when is_tuple(E) ->
    do_insert(tuple_to_list(E), Table, S).

do_insert([_EName | AllVals], Table, S) ->
    SkipFun = fun(#mekao_field{ro = RO}) -> RO end,
    {Fields, PHs, Types, Vals} = unpack(SkipFun, AllVals, Table, S),
    Q = [
        "INSERT INTO ", table(Table), " (",
            Fields,
        ") VALUES (",
            PHs,
        ") ",
        return(Table, S)
    ],
    {Q, Types, Vals}.


update_pk(E, Table, S) when is_tuple(E) ->
    SkipFun = fun(#mekao_field{key = Key}) -> not Key end,
    [EName | AllVals] = tuple_to_list(E),
    SkipKeysVals = lists:zipwith(
        fun
            (_, #mekao_field{key = true}) -> '$skip';
            (V, #mekao_field{key = false}) -> V
        end, AllVals, Table#mekao_table.fields
    ),
    do_update(SkipFun, [EName | SkipKeysVals], [EName | AllVals], Table, S).

-spec update(ToUpdate :: entity(), Condition :: entity(), table(), s()) -> 'query'().
update(E, C, Table, S) when is_tuple(E), is_tuple(C) ->
    SkipFun = fun(_) -> false end,
    do_update(SkipFun, tuple_to_list(E), tuple_to_list(C), Table, S).

do_update(SkipFun, [_EName | AllVals], [_EName | AllConds], Table, S) ->
    Fs = Table#mekao_table.fields,

    SetQData = {_, _, SetTypes, SetVals} =
        unpack(
            fun(#mekao_field{ro = RO}) -> RO end,
            1, AllVals, Fs, S
        ),

    WhereQData = {_, _, WhereTypes, WhereVals} =
        unpack(SkipFun, length(SetVals) + 1, AllConds, Fs, S),

    Q = [
        "UPDATE ",
            table(Table),
        " SET ",
            join_conditions(SetQData, ", "),
        " WHERE ",
            join_conditions(WhereQData, " AND "),
        return(Table, S)
    ],
    {Q, SetTypes ++ WhereTypes, SetVals ++ WhereVals}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

table(#mekao_table{name = Name}) ->
    Name.

-spec unpack( fun((field()) -> boolean())
            , [value()], table(), s()
            ) -> fphtvs().
unpack(SkipFun, Vs, #mekao_table{fields = Fs}, S) ->
    unpack(SkipFun, 1, Vs, Fs, S).

unpack(_, _, [], [], _) ->
    {[], [], [], []};

unpack(SkipFun, Num, ['$skip' | Vs], [_ | Fs], S) ->
    unpack(SkipFun, Num, Vs, Fs, S);
unpack(SkipFun, Num, [V | Vs], [F | Fs], S) ->
    Skip = SkipFun(F),
    if not Skip ->
        #mekao_field{type = T, name = FName} = F,
        PH = (S#mekao_settings.ph_fun)(Num, V),
        {ResFs, ResPHs, ResTs, ResVs} = unpack(SkipFun, Num + 1, Vs, Fs, S),
        {[FName | ResFs], [PH | ResPHs], [T | ResTs], [V | ResVs]};
    true ->
        unpack(SkipFun, Num, Vs, Fs, S)
    end.


-spec return(table(), s()) -> iolist().
return(Table, #mekao_settings{ret_fun = RetFun}) ->
    RetFun(Table).

-spec fields(table() | [field()]) -> iolist().
fields(#mekao_table{fields = Fields}) ->
    fields(Fields);
fields([F]) ->
    [field(F)];
fields([F | Fs]) ->
    [field(F), ", "  | fields(Fs)].

field(#mekao_field{name = Name}) ->
    Name.


join_conditions({[], [], [], []}, _Sep) ->
    [];
join_conditions({[F], [PH], [T], [V]}, _Sep) ->
    [condition(F, PH, T, V)];
join_conditions({[F | Fs], [PH | PHs], [T | Ts], [V | Vs]}, Sep) ->
    [condition(F, PH, T, V), Sep | join_conditions({Fs, PHs, Ts, Vs}, Sep)].

condition(F, _PH, _T, undefined) ->
    [F, " IS NULL"];
condition(F, PH, _T, _V) ->
    [F, " = ", PH].

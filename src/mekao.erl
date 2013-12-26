-module(mekao).

%% API
-export([
    select_pk/3, select/3,
    insert/3,
    update_pk/3,
    delete_pk/3,

    build/1
]).

-include("mekao.hrl").

-type entity()  :: tuple().

-type table()   :: #mekao_table{}.
-type column()  :: #mekao_column{}.
-type s()       :: #mekao_settings{}.

-export_type([
    table/0,
    column/0,
    s/0
]).

-type qdata() :: {
    Columns      :: [iolist()],
    Placeholders :: [iolist()],
    Types        :: [term()],
    Vals         :: [term()]
}.

-type 'query'() :: #mekao_insert{} | #mekao_select{} | #mekao_update{}
                 | #mekao_delete{}.

-type query_ret() :: {'query'(), Types :: [term()], Vals :: [term()]}.


%% ===================================================================
%% API functions
%% ===================================================================

-spec insert(Entity :: tuple() | list(), table(), s()) -> query_ret().
%% @doc Inserts entity, omits columns with `$skip' value.
insert(E, Table, S) ->
    prepare_insert(
        qdata(
            fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
            1, E, Table, S
        ), Table, S
    ).

-spec select_pk(Entity :: tuple() | list(), table(), s()) -> query_ret().
%% @doc Reads entity by it's primary key.
select_pk(E, Table, S) ->
    prepare_select(
        qdata(
            fun(#mekao_column{key = Key}, _) -> not Key end,
            1, E, Table, S
        ), Table, S
    ).


-spec update_pk(Entity :: tuple() | list(), table(), s()) -> query_ret().
%% @doc Updates entity by it's primary key, omits columns with `$skip' value.
update_pk(E, Table, S) ->
    SetQData = {_, SetPHs, _, _} = qdata(
        fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
        1, E, Table, S
    ),
    WhereQData = qdata(
        fun(#mekao_column{key = Key}, _) -> not Key end,
        length(SetPHs) + 1, E, Table, S
    ),
    prepare_update(SetQData, WhereQData, Table, S).


-spec delete_pk(Entity :: tuple() | list(), table(), s()) -> query_ret().
%% @doc Deletes entity by primary key.
delete_pk(E, Table, S) ->
    prepare_delete(
        qdata(
            fun(#mekao_column{key = Key}, _) -> not Key end,
            1, E, Table, S
        ), Table, S
    ).


-spec select(Entity :: tuple() | list(), table(), s()) -> query_ret().
%% @doc Selects several entities, omits columns with `$skip' value.
select(E, Table, S) ->
    prepare_select(
        qdata(
            fun(_, V) -> V == '$skip' end,
            1, E, Table, S
        ), Table, S
    ).


-spec build('query'()) -> iolist().
build(Select) when is_record(Select, mekao_select) ->
    #mekao_select{
        columns = Columns,
        table   = Table,
        where   = Where
    } = Select,

    [<<"SELECT ">>, Columns, <<" FROM ">>, Table, build_where(Where), <<";">>];

build(Insert) when is_record(Insert, mekao_insert) ->
    #mekao_insert{
        table        = Table,
        columns      = Columns,
        values       = Values,
        returning    = Return
    } = Insert,
    [
        <<"INSERT INTO ">>, Table, <<" (">>, Columns, <<") VALUES (">>,
        Values, <<")">>, build_return(Return), <<";">>
    ];

build(Update) when is_record(Update, mekao_update) ->
    #mekao_update{
        table       = Table,
        set         = Set,
        where       = Where,
        returning   = Return
    } = Update,
    [
        <<"UPDATE ">>, Table, <<" SET ">>, Set,
        build_where(Where), build_return(Return), <<";">>
    ];

build(Delete) when is_record(Delete, mekao_delete) ->
    #mekao_delete{
        table       = Table,
        where       = Where,
        returning   = Return
    } = Delete,
    [<<"DELETE FROM ">>, Table, build_where(Where), build_return(Return)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec prepare_insert(qdata(), table(), s()) -> query_ret().
prepare_insert({Cols, PHs, Types, Vals}, Table, S) ->
    Q = #mekao_insert{
        table       = Table#mekao_table.name,
        columns     = mekao_utils:intersperse(Cols, <<", ">>),
        values      = mekao_utils:intersperse(PHs, <<", ">>),
        returning   = returning(insert, Table, S)
    },
    {Q, Types, Vals}.

-spec prepare_select(qdata(), table(), s()) -> query_ret().
prepare_select(QData = {_, _, Types, Vals}, Table, S) ->
    Q = #mekao_select{
        table       = Table#mekao_table.name,
        columns     = all_columns(Table),
        where       = where(QData, S)
    },
    {Q, Types, Vals}.

-spec prepare_update(qdata(), qdata(), table(), s()) -> query_ret().
prepare_update( {_, _, SetTypes, SetVals} = SetQData
              , {_, _, WhereTypes, WhereVals} = WhereQData
              , Table, S) ->
    Q = #mekao_update{
        table       = Table#mekao_table.name,
        set         = set(SetQData),
        where       = where(WhereQData, S),
        returning   = returning(update, Table, S)
    },
    {Q, SetTypes ++ WhereTypes, SetVals ++ WhereVals}.

-spec prepare_delete(qdata(), table(), s()) -> query_ret().
prepare_delete(QData = {_, _, Types, Vals}, Table, S) ->
    Q = #mekao_delete{
        table       = Table#mekao_table.name,
        where       = where(QData, S),
        returning   = returning(delete, Table, S)
    },
    {Q, Types, Vals}.


-spec qdata( fun((column(), Val :: term()) -> boolean())
           , non_neg_integer()
           , entity() | [term()]
           , table()  | [column()]
           , s()
           ) -> qdata().
qdata(SkipFun, Num, E, Table, S) when is_tuple(E) ->
    [_EntityName | AllVals] = tuple_to_list(E),
    qdata(SkipFun, Num, AllVals, Table, S);

qdata(SkipFun, Num, Vals, #mekao_table{columns = Cols}, S) ->
    qdata(SkipFun, Num, Vals, Cols, S);

qdata(_, _, [], [], _) ->
    {[], [], [], []};

qdata(SkipFun, Num, [V | Vals], [Col | Cols], S) ->
    Skip = SkipFun(Col, V),
    if not Skip ->
        #mekao_column{type = T, name = CName} = Col,
        PH = (S#mekao_settings.placeholder)(Num, V),

        {ResCols, ResPHs, ResTypes, ResVals} = qdata(
            SkipFun, Num + 1, Vals, Cols, S
        ),
        {[CName | ResCols], [PH | ResPHs], [T | ResTypes], [V | ResVals]};
    true ->
        qdata(SkipFun, Num, Vals, Cols, S)
    end.


-spec returning(insert | update | delete, table(), s()) -> iolist().
returning(QType, Table, #mekao_settings{returning = RetFun}) ->
    RetFun(QType, Table).


-spec all_columns(table() | [column()]) -> iolist().
all_columns(#mekao_table{columns = Columns}) ->
    all_columns(Columns);
all_columns(Columns) ->
    mekao_utils:intersperse(
        Columns, <<", ">>, fun(#mekao_column{name = Name}) -> Name end
    ).


where(QData, #mekao_settings{is_null = IsNull}) ->
    mekao_utils:intersperse4(
        QData,
        <<" AND ">>,
        fun ({C, PH, _T, V}) ->
            case IsNull(V) of
                false -> [C, <<" = ">>, PH];
                true -> [C, <<" IS NULL">>]
            end
        end
    ).

build_return([]) ->
    <<>>;
build_return(Return) ->
    [<<" ">> | Return].

build_where([]) ->
    <<>>;
build_where(Where) ->
    [<<" WHERE ">> | Where].

set(QData) ->
    mekao_utils:intersperse4(
        QData, <<", ">>,
        fun ({C, PH, _T, _V}) -> [C, <<" = ">>, PH] end
    ).

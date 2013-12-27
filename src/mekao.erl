-module(mekao).

%% API
-export([
    select_pk/3, select/3,
    insert/3,
    update_pk/3,
    delete_pk/3,

    prepare/4,
    build/1
]).

-include("mekao.hrl").

-type table()   :: #mekao_table{}.
-type column()  :: #mekao_column{}.
-type s()       :: #mekao_settings{}.

%% generic query
-type 'query'(Body) :: #mekao_query{body :: Body}.

%% prepared query
-type p_query() :: 'query'( #mekao_insert{}
                          | #mekao_select{}
                          | #mekao_update{}
                          | #mekao_delete{}
                          ).
%% builded query
-type b_query() :: 'query'(iolist()).

-export_type([
    table/0,
    column/0,
    s/0,
    p_query/0,
    b_query/0
]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec insert(Entity :: tuple() | list(), table(), s()) -> b_query().
%% @doc Inserts entity, omits columns with `$skip' value.
insert(E, Table, S) ->
    build(prepare(insert, E, Table, S)).


-spec select_pk(Entity :: tuple() | list(), table(), s()) -> b_query().
%% @doc Reads entity by it's primary key.
select_pk(E, Table, S) ->
    build(prepare(select_pk, E, Table, S)).


-spec select(Entity :: tuple() | list(), table(), s()) -> b_query().
%% @doc Selects several entities, omits columns with `$skip' value.
select(E, Table, S) ->
    build(prepare(select, E, Table, S)).


-spec update_pk(Entity :: tuple() | list(), table(), s()) -> b_query().
%% @doc Updates entity by it's primary key, omits columns with `$skip' value.
update_pk(E, Table, S) ->
    build(prepare(update_pk, E, Table, S)).


-spec delete_pk(Entity :: tuple() | list(), table(), s()) -> b_query().
%% @doc Deletes entity by primary key.
delete_pk(E, Table, S) ->
    build(prepare(delete_pk, E, Table, S)).


-spec prepare( insert | select_pk | select | update_pk | delete_pk
             , Entity :: tuple() | list()
             , table(), s()
             ) -> p_query().
prepare(insert, E, Table, S) ->
    SkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    prepare(insert, SkipFun, E, Table, S);

prepare(select_pk, E, Table, S) ->
    SkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,
    prepare(select, SkipFun, E, Table, S);

prepare(select, E, Table, S) ->
    SkipFun = fun(_, V) -> V == '$skip' end,
    prepare(select, SkipFun, E, Table, S);

prepare(update_pk, E, Table, S) ->
    SetSkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    WhereSkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,
    prepare(update, {SetSkipFun, WhereSkipFun}, {E, E}, Table, S);

prepare(delete_pk, E, Table, S) ->
    SkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,
    prepare(delete, SkipFun, E, Table, S).


-spec build(p_query()) -> b_query().
build(Q = #mekao_query{body = Select}) when is_record(Select, mekao_select) ->
    #mekao_select{
        columns = Columns,
        table   = Table,
        where   = Where
    } = Select,
    Q#mekao_query{
        body = [
            <<"SELECT ">>, Columns, <<" FROM ">>, Table,
            build_where(Where), <<";">>
        ]
    };

build(Q = #mekao_query{body = Insert}) when is_record(Insert, mekao_insert) ->
    #mekao_insert{
        table        = Table,
        columns      = Columns,
        values       = Values,
        returning    = Return
    } = Insert,
    Q#mekao_query{
        body = [
            <<"INSERT INTO ">>, Table, <<" (">>, Columns, <<") VALUES (">>,
            Values, <<")">>, build_return(Return), <<";">>
        ]
    };

build(Q = #mekao_query{body = Update}) when is_record(Update, mekao_update) ->
    #mekao_update{
        table       = Table,
        set         = Set,
        where       = Where,
        returning   = Return
    } = Update,
    Q#mekao_query{
        body = [
            <<"UPDATE ">>, Table, <<" SET ">>, Set,
            build_where(Where), build_return(Return), <<";">>
        ]
    };

build(Q = #mekao_query{body = Delete}) when is_record(Delete, mekao_delete) ->
    #mekao_delete{
        table       = Table,
        where       = Where,
        returning   = Return
    } = Delete,
    Q#mekao_query{
        body = [
            <<"DELETE FROM ">>, Table, build_where(Where),
            build_return(Return), <<";">>
        ]
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

prepare(insert, SkipFun, E, Table, S) ->
    {Cols, PHs, Types, Vals} = qdata(
        SkipFun, 1, E, Table, S
    ),
    Q = #mekao_insert{
        table       = Table#mekao_table.name,
        columns     = mekao_utils:intersperse(Cols, <<", ">>),
        values      = mekao_utils:intersperse(PHs, <<", ">>),
        returning   = returning(insert, Table, S)
    },
    #mekao_query{
       body     = Q,
       types    = Types,
       values   = Vals
    };

prepare(select, SkipFun, E, Table, S) ->
    QData = {_, _, Types, Vals} = qdata(
        SkipFun, 1, E, Table, S
    ),
    Q = #mekao_select{
        table       = Table#mekao_table.name,
        columns     = all_columns(Table),
        where       = where(QData, S)
    },
    #mekao_query{
       body     = Q,
       types    = Types,
       values   = Vals
    };

prepare(update, {SetSkipFun, WhereSkipFun}, {SetE, WhereE}, Table, S) ->
    SetQData = {_, SetPHs, SetTypes, SetVals} = qdata(
        SetSkipFun, 1, SetE, Table, S
    ),
    WhereQData = {_, _, WhereTypes, WhereVals} = qdata(
        WhereSkipFun, length(SetPHs) + 1, WhereE, Table, S
    ),
    Q = #mekao_update{
        table       = Table#mekao_table.name,
        set         = set(SetQData),
        where       = where(WhereQData, S),
        returning   = returning(update, Table, S)
    },
    #mekao_query{
       body     = Q,
       types    = SetTypes ++ WhereTypes,
       values   = SetVals ++ WhereVals
    };

prepare(delete, SkipFun, E, Table, S) ->
    QData = {_, _, Types, Vals} = qdata(
        SkipFun, 1, E, Table, S
    ),
    Q = #mekao_delete{
        table       = Table#mekao_table.name,
        where       = where(QData, S),
        returning   = returning(delete, Table, S)
    },
    #mekao_query{
       body     = Q,
       types    = Types,
       values   = Vals
    }.


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
        #mekao_column{type = T, name = CName, transform = TrFun} = Col,
        NewV =
            if TrFun == undefined ->
                V;
            true ->
                TrFun(V)
            end,
        PH = (S#mekao_settings.placeholder)(Col, Num, NewV),
        {ResCols, ResPHs, ResTypes, ResVals} = qdata(
            SkipFun, Num + 1, Vals, Cols, S
        ),
        {[CName | ResCols], [PH | ResPHs], [T | ResTypes], [NewV | ResVals]};
    true ->
        qdata(SkipFun, Num, Vals, Cols, S)
    end.


-spec returning(insert | update | delete, table(), s()) -> iolist().
returning(_QType, _Table, #mekao_settings{returning = undefined}) ->
    [];
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

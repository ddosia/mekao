-module(mekao).

%% API
-export([
    select_pk/3, select/3,
    insert/3,
    update_pk/3,
    update_pk_diff/3,
    delete_pk/3,

    prepare_select/3,
    prepare_insert/3,
    prepare_update/4,
    prepare_delete/3,
    build/1
]).

-include("mekao.hrl").

-type table()   :: #mekao_table{}.
-type column()  :: #mekao_column{}.
-type s()       :: #mekao_settings{}.

-type entity()      :: tuple() | list().
-type selector()    :: tuple() | list(predicate()).

-type predicate()   :: Value :: term()
                     | { '$predicate'
                       , '=' | '<>' | '>' | '>=' | '<' | '<='
                       , Value :: term() }.

%% generic query
-type 'query'(Body) :: #mekao_query{body :: Body}.

%% prepared query
-type p_query() :: 'query'( #mekao_insert{}
                          | #mekao_select{}
                          | #mekao_update{}
                          | #mekao_delete{}
                          ).
%% built query
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

-spec insert(entity(), table(), s()) -> b_query().
%% @doc Inserts entity, omits columns with `$skip' value.
insert(E, Table, S) ->
    SkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    build(prepare_insert(
        skip(e2l(E), Table#mekao_table.columns, SkipFun), Table, S
    )).


-spec select_pk(selector(), table(), s()) -> b_query().
%% @doc Reads entity by it's primary key.
select_pk(E, Table, S) ->
    SkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,
    build(prepare_select(
        skip(e2l(E), Table#mekao_table.columns, SkipFun), Table, S
    )).


-spec select(selector(), table(), s()) -> b_query().
%% @doc Selects several entities, omits columns with `$skip' value.
select(E, Table, S) ->
    SkipFun = fun(_, V) -> V == '$skip' end,
    build(prepare_select(
        skip(e2l(E), Table#mekao_table.columns, SkipFun), Table, S
    )).


-spec update_pk(entity(), table(), s()) -> b_query().
%% @doc Updates entity by it's primary key, omits columns with `$skip' value.
update_pk(E, Table = #mekao_table{columns = MekaoCols}, S) ->

    SetSkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    WhereSkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,

    Vals = e2l(E),

    build(prepare_update(
        skip(Vals, MekaoCols, SetSkipFun), skip(Vals, MekaoCols, WhereSkipFun),
        Table, S
    )).


-spec update_pk_diff( {Old :: entity(), New :: entity()}, table(), s()
                    ) -> b_query().
%% @doc Updates only changed fields by primary key.
update_pk_diff({E1, E2}, Table, S) ->
    EDiff =
        fun
            (V,  V,  #mekao_column{key = true}) -> V;
            (V,  V,  #mekao_column{key = false}) -> '$skip';
            (V1, V2, #mekao_column{key = false}) when V1 /= V2 -> V2
        end,
    update_pk(
        mekao_utils:map3(EDiff, e2l(E1), e2l(E2), Table#mekao_table.columns),
        Table, S
    ).


-spec delete_pk(selector(), table(), s()) -> b_query().
%% @doc Deletes entity by primary key.
delete_pk(E, Table, S) ->
    SkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,
    build(prepare_delete(
        skip(e2l(E), Table#mekao_table.columns, SkipFun), Table, S
    )).


-spec prepare_insert(entity(), table(), s()) -> p_query().
prepare_insert(E, Table, S) ->
    {Cols, PHs, Types, Vals} = qdata(1, e2l(E), Table#mekao_table.columns, S),
    Q = #mekao_insert{
        table       = Table#mekao_table.name,
        columns     = mekao_utils:intersperse(Cols, <<", ">>),
        values      = mekao_utils:intersperse(PHs, <<", ">>),
        returning   = returning(insert, Table, S)
    },
    #mekao_query{
       body     = Q,
       types    = Types,
       values   = Vals,
       next_ph_num = length(PHs) + 1
    }.


-spec prepare_select(selector(), table(), s()) -> p_query().
prepare_select(E, Table = #mekao_table{columns = MekaoCols}, S) ->
    {Where, {_, PHs, Types, Vals}} = where(
        qdata(1, e2l(E), MekaoCols, S), S
    ),
    AllCols = mekao_utils:intersperse(
        MekaoCols, <<", ">>, fun(#mekao_column{name = Name}) -> Name end
    ),

    Q = #mekao_select{
        table       = Table#mekao_table.name,
        columns     = AllCols,
        where       = Where
    },
    #mekao_query{
       body     = Q,
       types    = Types,
       values   = Vals,
       next_ph_num = length(PHs) + 1
    }.


-spec prepare_update(entity(), selector(), table(), s()) -> p_query().
prepare_update(SetE, WhereE, Table = #mekao_table{columns = MekaoCols}, S) ->
    SetQData = {_, SetPHs, SetTypes, SetVals} = qdata(
        1, e2l(SetE), MekaoCols, S
    ),
    SetPHsLen = length(SetPHs),

    {Where, {_, WherePHs, WhereTypes, WhereVals}}
        = where(qdata(SetPHsLen + 1, e2l(WhereE), MekaoCols, S), S),

    WherePHsLen = length(WherePHs),

    Q = #mekao_update{
        table       = Table#mekao_table.name,
        set         = set(SetQData),
        where       = Where,
        returning   = returning(update, Table, S)
    },
    #mekao_query{
       body     = Q,
       types    = SetTypes ++ WhereTypes,
       values   = SetVals ++ WhereVals,
       next_ph_num = SetPHsLen + WherePHsLen + 1
    }.


-spec prepare_delete(selector(), table(), s()) -> p_query().
prepare_delete(E, Table, S) ->
    {Where, {_, PHs, Types, Vals}}
        = where(qdata(1, e2l(E), Table#mekao_table.columns, S), S),

    Q = #mekao_delete{
        table       = Table#mekao_table.name,
        where       = Where,
        returning   = returning(delete, Table, S)
    },
    #mekao_query{
       body     = Q,
       types    = Types,
       values   = Vals,
       next_ph_num = length(PHs) + 1
    }.


-spec build(p_query()) -> b_query().
build(Q = #mekao_query{body = Select}) when is_record(Select, mekao_select) ->
    #mekao_select{
        columns = Columns,
        table   = Table,
        where   = Where
    } = Select,
    Q#mekao_query{
        body = [
            <<"SELECT ">>, Columns, <<" FROM ">>, Table, build_where(Where)
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
            Values, <<")">>, build_return(Return)
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
            build_where(Where), build_return(Return)
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
            build_return(Return)
        ]
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc entity to list
e2l(Vals) when is_list(Vals) ->
    Vals;
e2l(E) when is_tuple(E) ->
    [_EntityName | Vals] = tuple_to_list(E),
    Vals.


skip([], [], _SkipFun) ->
    [];

skip([V | Vals], [C | Cols], SkipFun) ->
    NewV =
        case SkipFun(C, V) of
            true ->
                '$skip';
            false ->
                V
        end,
    [NewV | skip(Vals, Cols, SkipFun)].


qdata(_, [], [], _) ->
    {[], [], [], []};

qdata(Num, ['$skip' | Vals], [_Col | Cols], S) ->
    qdata(Num, Vals, Cols, S);

qdata(Num, [Pred | Vals], [Col | Cols], S) ->
    #mekao_column{type = T, name = CName, transform = TrFun} = Col,

    V = predicate_val(Pred),
    NewV =
        if TrFun == undefined ->
            V;
        true ->
            TrFun(V)
        end,
    PH = (S#mekao_settings.placeholder)(Col, Num, NewV),
    NewPred = set_predicate_val(Pred, NewV),
    {ResCols, ResPHs, ResTypes, ResVals} = qdata(
        Num + 1, Vals, Cols, S
    ),
    {[CName | ResCols], [PH | ResPHs], [T | ResTypes], [NewPred | ResVals]}.


-spec returning(insert | update | delete, table(), s()) -> iolist().
returning(_QType, _Table, #mekao_settings{returning = undefined}) ->
    [];
returning(QType, Table, #mekao_settings{returning = RetFun}) ->
    RetFun(QType, Table).


where(QData = {[], [], [], []}, _S) ->
    {[], QData};

where({[C], [PH], [T], [V]}, S) ->
    {W, {NewC, NewPH, NewT, NewV}} = predicate({C, PH, T, V}, S),
    {[W], {[NewC], [NewPH], [NewT], [NewV]}};

where({[C | Cs], [PH | PHs], [T | Types], [V | Vals]}, S) ->
    {W, {NewC, NewPH, NewT, NewV}} = predicate({C, PH, T, V}, S),
    {Ws, {NewCs, NewPHs, NewTypes, NewVals}} = where({Cs, PHs, Types, Vals}, S),
    {[W, <<" AND ">> | Ws],
        {[NewC | NewCs], [NewPH | NewPHs], [NewT | NewTypes], [NewV | NewVals]}}.

%% TODO: add NOT, IN, ANY, ALL, BETWEEN, LIKE handling
predicate({C, PH, T, {'$predicate', Op, V}}, S) when Op == '='; Op == '<>' ->
    IsNull = (S#mekao_settings.is_null)(V),
    if not IsNull ->
        {[C, op_to_bin(Op), PH], {C, PH, T, V}};
    Op == '=' ->
        {[C, <<" IS NULL">>], {C, PH, T, V}};
    Op == '<>' ->
        {[C, <<" IS NOT NULL">>], {C, PH, T, V}}
    end;
predicate({C, PH, T, {'$predicate', OP, V}},  _S) ->
    {[C, op_to_bin(OP), PH],  {C, PH, T, V}};
predicate({C, PH, T, V}, S) ->
    predicate({C, PH, T, {'$predicate', '=', V}}, S).

op_to_bin('=')  -> <<" = ">>;
op_to_bin('<>') -> <<" <> ">>;
op_to_bin('>')  -> <<" > ">>;
op_to_bin('>=') -> <<" >= ">>;
op_to_bin('<')  -> <<" < ">>;
op_to_bin('<=') -> <<" <= ">>.

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

predicate_val({'$predicate', _, V}) ->
    V;
predicate_val(V) ->
    V.

set_predicate_val({'$predicate', Op, _}, NewV) ->
    {'$predicate', Op, NewV};
set_predicate_val(_, NewV) ->
    NewV.

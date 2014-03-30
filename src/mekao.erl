-module(mekao).

%% API
-export([
    select_pk/3, select/3,
    insert/3,
    update_pk/3,
    update_pk_diff/4,
    update/4,
    delete_pk/3,
    delete/3,

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

-type entity()      :: tuple() | list(term() | '$skip').
-type selector()    :: tuple() | list(predicate()).

-type predicate() :: term()
                  | { '$predicate', between, term(), term() }
                  | { '$predicate'
                    , '=' | '<>' | '>' | '>=' | '<' | '<=' | like
                    , term()
                    }.

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
    table/0, column/0, s/0,
    p_query/0, b_query/0,
    predicate/0
]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec insert(entity(), table(), s()) -> {ok, b_query()}
                                      | {error, empty_insert}.
%% @doc Inserts entity, omits columns with `$skip' value.
insert(E, Table, S) ->
    SkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    Q = prepare_insert(
        skip(SkipFun, Table#mekao_table.columns, e2l(E)), Table, S
    ),
    if Q#mekao_query.values /= [] ->
        {ok, build(Q)};
    true ->
        {error, empty_insert}
    end.


-spec select_pk(selector(), table(), s()) -> {ok, b_query()}
                                           | {error, pk_miss}.
%% @doc Reads entity by it's primary key.
select_pk(E, Table, S) ->
    SkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,
    Q = prepare_select(
        skip(SkipFun, Table#mekao_table.columns, e2l(E)), Table, S
    ),
    if Q#mekao_query.values /= [] ->
        {ok, build(Q)};
    true ->
        {error, pk_miss}
    end.


-spec select(selector(), table(), s()) -> {ok, b_query()}.
%% @doc Selects several entities, omits columns with `$skip' value.
select(E, Table, S) ->
    SkipFun = fun(_, V) -> V == '$skip' end,
    {ok, build(prepare_select(
        skip(SkipFun, Table#mekao_table.columns, e2l(E)), Table, S
    ))}.


-spec update_pk(selector(), table(), s()) -> {ok, b_query()}
                                           | {error, pk_miss}
                                           | {error, empty_update}.
%% @doc Updates entity by it's primary key, omits columns with `$skip' value.
update_pk(E, Table = #mekao_table{columns = MekaoCols}, S) ->

    SetSkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    WhereSkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,

    Vals = e2l(E),
    Q = prepare_update(
        skip(SetSkipFun, MekaoCols, Vals), skip(WhereSkipFun, MekaoCols, Vals),
        Table, S
    ),
    if (Q#mekao_query.body)#mekao_update.set == [] ->
        {error, empty_update};
    (Q#mekao_query.body)#mekao_update.where == [] ->
        {error, pk_miss};
    true ->
        {ok, build(Q)}
    end.


-spec update_pk_diff( Old :: entity(), New :: entity(), table(), s()
                    ) -> {ok, b_query()}
                       | {error, pk_miss}
                       | {error, empty_update}.
%% @doc Updates only changed fields by primary key.
update_pk_diff(E1, E2, Table = #mekao_table{columns = MekaoCols}, S) ->
    Vals1 = e2l(E1),
    Vals2 = e2l(E2),
    DiffVals = mekao_utils:map2(
        fun
            (V, V) -> '$skip';
            (_, V2) -> V2
        end, Vals1, Vals2
    ),
    SetSkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    WhereSkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,

    Q = prepare_update(
        skip(SetSkipFun, MekaoCols, DiffVals),
        skip(WhereSkipFun, MekaoCols, Vals1),
        Table, S
    ),

    if (Q#mekao_query.body)#mekao_update.set == [] ->
        {error, empty_update};
    (Q#mekao_query.body)#mekao_update.where == [] ->
        {error, pk_miss};
    true ->
        {ok, build(Q)}
    end.


-spec update(entity(), selector(), table(), s()) -> {ok, b_query()}
                                                  | {error, empty_update}.
%% @doc Updates entities, composes WHERE clause from `Selector'
%%      non `$skip' fields.
update(E, Selector, Table = #mekao_table{columns = MekaoCols}, S) ->
    SetSkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    WhereSkipFun = fun(_, V) -> V == '$skip' end,

    Q = prepare_update(
        skip(SetSkipFun, MekaoCols, e2l(E)),
        skip(WhereSkipFun, MekaoCols, e2l(Selector)),
        Table, S
    ),
    if (Q#mekao_query.body)#mekao_update.set == [] ->
        {error, empty_update};
    true ->
        {ok, build(Q)}
    end.


-spec delete_pk(selector(), table(), s()) -> {ok, b_query()} | {error, pk_miss}.
%% @doc Deletes entity by primary key.
delete_pk(E, Table, S) ->
    SkipFun = fun(#mekao_column{key = Key}, _) -> not Key end,
    Q = prepare_delete(
        skip(SkipFun, Table#mekao_table.columns, e2l(E)), Table, S
    ),
    if Q#mekao_query.values /= [] ->
        {ok, build(Q)};
    true ->
        {error, pk_miss}
    end.


-spec delete(selector(), table(), s()) -> {ok, b_query()}.
%% @doc Deletes entities, composes WHERE clause  from `Selector'
%%      non `$skip' fields.
delete(Selector, Table, S) ->
    SkipFun = fun(_, V) -> V == '$skip' end,

    Q = prepare_delete(
        skip(SkipFun, Table#mekao_table.columns, e2l(Selector)), Table, S
    ),
    {ok, build(Q)}.


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
prepare_select(E, Table, S) ->
    #mekao_table{
        columns = MekaoCols,
        order_by = OrderBy
    } = Table,

    {Where, {PHs, Types, Vals}} = where(
        qdata(1, e2l(E), MekaoCols, S), S
    ),
    AllCols = mekao_utils:intersperse(
        MekaoCols, <<", ">>, fun(#mekao_column{name = Name}) -> Name end
    ),

    Q = #mekao_select{
        table       = Table#mekao_table.name,
        columns     = AllCols,
        where       = Where,
        order_by    = order_by(OrderBy)
    },
    #mekao_query{
       body     = Q,
       types    = Types,
       values   = Vals,
       next_ph_num = length(PHs) + 1
    }.


-spec prepare_update(entity(), selector(), table(), s()) -> p_query().
prepare_update(SetE, WhereE, Table = #mekao_table{columns = MekaoCols}, S) ->
    {SetCols, SetPHs, SetTypes, SetVals} = qdata(
        1, e2l(SetE), MekaoCols, S
    ),
    SetPHsLen = length(SetPHs),

    {Where, {WherePHs, WhereTypes, WhereVals}}
        = where(qdata(SetPHsLen + 1, e2l(WhereE), MekaoCols, S), S),

    WherePHsLen = length(WherePHs),

    Set = mekao_utils:intersperse2(
        fun (C, PH) -> [C, <<" = ">>, PH] end,
        <<", ">>, SetCols, SetPHs
    ),

    Q = #mekao_update{
        table       = Table#mekao_table.name,
        set         = Set,
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
    {Where, {PHs, Types, Vals}}
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
        where   = Where,
        order_by = OrderBy
    } = Select,
    Q#mekao_query{
        body = [
            <<"SELECT ">>, Columns, <<" FROM ">>, Table, build_where(Where),
            build_order_by(OrderBy)
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


skip(SkipFun, Cols, Vals) ->
    mekao_utils:map2(
        fun(C, V) ->
            Skip = SkipFun(C, V),
            if  Skip -> '$skip';
                true    -> V
            end
        end, Cols, Vals
    ).


qdata(_, [], [], _) ->
    {[], [], [], []};

qdata(Num, ['$skip' | Vals], [_Col | Cols], S) ->
    qdata(Num, Vals, Cols, S);

qdata(Num, [Pred | Vals], [Col | Cols], S) ->
    #mekao_settings{placeholder = PHFun} = S,
    #mekao_column{type = T, name = CName, transform = TrFun} = Col,

    {NextNum, NewPH, NewT, NewV} =
        case Pred of
            {'$predicate', Op, V} ->
                TransV = transform(TrFun, V),
                PH = PHFun(Col, Num, TransV),
                {Num + 1, PH, T, {'$predicate', Op, TransV}};
            {'$predicate', 'between', V1, V2} ->
                TransV1 = transform(TrFun, V1),
                TransV2 = transform(TrFun, V2),
                PH1 = PHFun(Col, Num, TransV1),
                PH2 = PHFun(Col, Num + 1, TransV2),
                {Num + 2, {PH1, PH2}, T,
                    {'$predicate', 'between', TransV1, TransV2}
                };
            V ->
                TransV = transform(TrFun, V),
                PH = PHFun(Col, Num, TransV),
                {Num + 1, PH, T, TransV}
        end,
    {ResCols, ResPHs, ResTypes, ResVals} = qdata(
        NextNum, Vals, Cols, S
    ),

    {[CName | ResCols], [NewPH | ResPHs], [NewT | ResTypes],
        [NewV | ResVals]}.


-spec returning(insert | update | delete, table(), s()) -> iolist().
returning(_QType, _Table, #mekao_settings{returning = undefined}) ->
    [];
returning(QType, Table, #mekao_settings{returning = RetFun}) ->
    RetFun(QType, Table).


where({[], [], [], []}, _S) ->
    {[], {[], [], []}};

where({[C], [PH], [T], [V]}, S) ->
    {W, {NewPHs, NewTs, NewVs}} = predicate({C, PH, T, V}, S),
    {[W], {NewPHs, NewTs, NewVs}};

where({[C | Cs], [PH | PHs], [T | Types], [V | Vals]}, S) ->
    {W, {NewPHs, NewTs, NewVs}} = predicate({C, PH, T, V}, S),
    {Ws, {ResPHs, ResTs, ResVs}} = where({Cs, PHs, Types, Vals}, S),
    {[W, <<" AND ">> | Ws],
        {NewPHs ++ ResPHs, NewTs ++ ResTs, NewVs ++ ResVs}}.


%% TODO: add NOT, IN, ANY, ALL, BETWEEN handling
predicate({C, PH, T, {'$predicate', Op, V}}, S) when Op == '='; Op == '<>' ->
    IsNull = (S#mekao_settings.is_null)(V),
    if not IsNull ->
        {[C, op_to_bin(Op), PH], {[PH], [T], [V]}};
    Op == '=' ->
        {[C, <<" IS NULL">>], {[PH], [T], [V]}};
    Op == '<>' ->
        {[C, <<" IS NOT NULL">>], {[PH], [T], [V]}}
    end;

predicate({C, {PH1, PH2}, T, {'$predicate', between, V1, V2}}, _S) ->
    {[C, <<" BETWEEN ">>, PH1, <<" AND ">>, PH2], {[PH1, PH2], [T, T], [V1, V2]}};

predicate({C, PH, T, {'$predicate', like, V}},  _S) ->
    {[C, <<" LIKE ">>, PH], {[PH], [T], [V]}};

predicate({C, PH, T, {'$predicate', OP, V}},  _S) ->
    {[C, op_to_bin(OP), PH], {[PH], [T], [V]}};

predicate({C, PH, T, V}, S) ->
    predicate({C, PH, T, {'$predicate', '=', V}}, S).


op_to_bin('=')  -> <<" = ">>;
op_to_bin('<>') -> <<" <> ">>;
op_to_bin('>')  -> <<" > ">>;
op_to_bin('>=') -> <<" >= ">>;
op_to_bin('<')  -> <<" < ">>;
op_to_bin('<=') -> <<" <= ">>.


order_by([]) ->
    [];
order_by([O]) ->
    [order_by_1(O)];
order_by([O | OrderBy]) ->
    [order_by_1(O), <<", ">> | order_by(OrderBy)].

order_by_1(E) when not is_tuple(E) ->
    order_by_1({E, {default, default}});

order_by_1({Pos, Opts}) when is_integer(Pos) ->
    order_by_1({integer_to_list(Pos - 1), Opts});

order_by_1({Expr, Opts}) when is_list(Expr); is_binary(Expr) ->
    [Expr, order_by_opts(Opts)].

order_by_opts({Ordering, Nulls}) ->
    O = case Ordering of
        default ->
            <<"">>;
        asc ->
            <<" ASC">>;
        desc ->
            <<" DESC">>
    end,
    case Nulls of
        default ->
            O;
        nulls_first ->
            <<O/binary," NULLS FIRST">>;
        nulls_last ->
            <<O/binary, " NULLS LAST">>
    end.


build_return([]) ->
    <<>>;
build_return(Return) ->
    [<<" ">> | Return].

build_where([]) ->
    <<>>;
build_where(Where) ->
    [<<" WHERE ">> | Where].


build_order_by([]) ->
    <<>>;
build_order_by(OrderBy) ->
    [<<" ORDER BY ">>, OrderBy].


transform(undefined, V) ->
    V;
transform(TrFun, V) when is_function(TrFun, 1) ->
    TrFun(V).

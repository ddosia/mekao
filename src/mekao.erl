-module(mekao).

%% API
-export([
    select_pk/3, select/3, select/4,
    insert/3, insert_all/3,
    update_pk/3,
    update_pk_diff/4,
    update/4,
    delete_pk/3,
    delete/3,

    prepare_select/3, prepare_select/4,
    prepare_insert/3, prepare_insert_all/3,
    prepare_update/4,
    prepare_delete/3,
    build/1
]).

-include("mekao.hrl").

-type iotriple() :: iodata() | {iodata(), iodata(), iodata()}.

-type table()   :: #mekao_table{}.
-type column()  :: #mekao_column{}.
-type s()       :: #mekao_settings{}.

-type entity()      :: tuple() | list(term() | '$skip').
-type selector()    :: tuple() | list(predicate()).

-type predicate() :: term()
                  | { '$predicate', between, term(), term() }
                  | { '$predicate', in, [term(), ...] }
                  | { '$predicate'
                    , '=' | '<>' | '>' | '>=' | '<' | '<=' | like
                    , term()
                    }
                  | { '$predicate', 'not', predicate()}.

-type select_opt() :: {limit, { RowCount :: non_neg_integer()
                              , Offset   :: non_neg_integer()}}.

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
    iotriple/0,
    table/0, column/0, s/0,
    'query'/1, p_query/0, b_query/0,
    predicate/0
]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec insert( entity(), table(), s()
            ) -> {ok, b_query()} | {error, empty_insert}.
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


-spec insert_all( [entity(), ...], table(), s()
                ) -> {ok, b_query()} | {error, empty_insert}.
%% @doc Inserts entities, places `DEFAULT' keyword when column with `$skip'
%%      value occurs.
insert_all(Es = [_ | _], Table, S) ->
    SkipFun = fun(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip' end,
    Q = prepare_insert_all(
        [skip(SkipFun, Table#mekao_table.columns, e2l(E)) || E <- Es], Table, S
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
    select(E, [], Table, S).

-spec select(selector(), [select_opt()], table(), s()) -> {ok, b_query()}.
select(E, Opts, Table, S) ->
    SkipFun = fun(_, V) -> V == '$skip' end,
    {ok, build(prepare_select(
        skip(SkipFun, Table#mekao_table.columns, e2l(E)), Opts, Table, S
    ))}.


-spec update_pk(selector(), table(), s()) -> {ok, b_query()}
                                           | {error, pk_miss}
                                           | {error, empty_update}.
%% @doc Updates entity by it's primary key, omits columns with `$skip' value.
update_pk(E, Table = #mekao_table{columns = MekaoCols}, S) ->

    SetSkipFun =
        fun(#mekao_column{ro = RO, key = Key}, V) ->
            RO orelse V == '$skip' orelse Key
        end,
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
%% @doc Updates only changed fields by primary key. This is possible to update
%%      PK as well if it is not `ro = true'.
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
%%      non `$skip' fields. This is possible to update PK as well if it
%%      is not `ro = true'.
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
    {NextNum, {Cols, PHs, Types, Vals}} =
        qdata(1, e2l(E), Table#mekao_table.columns, S),
    Q = #mekao_insert{
        table   = Table#mekao_table.name,
        columns = mekao_utils:intersperse(Cols, <<", ">>),
        values  = [<<"(">>, mekao_utils:intersperse(PHs, <<", ">>), <<")">>],
        returning = returning(insert, Table, S)
    },
    #mekao_query{
        body    = Q,
        types   = Types,
        values  = Vals,
        next_ph_num = NextNum
    }.


-spec prepare_insert_all([entity(), ...], table(), s()) -> p_query().
prepare_insert_all(Es = [_ | _], Table, S) ->
    MekaoCols = Table#mekao_table.columns,
    {ResNum, {RevPHs, RevTypes, RevVals}} =
        lists:foldl(
            fun(E, {AccNum, {AccPHs, AccTypes, AccVals}}) ->
                {NextNum, {PHs, Types, Vals}} =
                    qdata_insert(AccNum, e2l(E), MekaoCols, S),
                {NextNum, {
                    [PHs | AccPHs],
                    lists:reverse(Types) ++ AccTypes,
                    lists:reverse(Vals) ++ AccVals
                }}
            end, {1, {[], [], []}}, Es
        ),

    Q = #mekao_insert{
        table = Table#mekao_table.name,
        columns =
            mekao_utils:intersperse(
                MekaoCols, <<", ">>,
                fun(#mekao_column{name = Name}) -> Name end
            ),
        values =
            mekao_utils:intersperse(
                lists:reverse(RevPHs), <<", ">>,
                fun(PHs) ->
                    [<<"(">>, mekao_utils:intersperse(PHs, <<", ">>), <<")">>]
                end
            ),
        returning = returning(insert, Table, S)
    },
    #mekao_query{
        body     = Q,
        types    = lists:reverse(RevTypes),
        values   = lists:reverse(RevVals),
        next_ph_num = ResNum
    }.


-spec prepare_select(selector(), table(), s()) -> p_query().
prepare_select(E, Table, S) ->
    prepare_select(E, [], Table, S).


-spec prepare_select(selector(), [select_opt()], table(), s()) -> p_query().
prepare_select(E, Opts, Table, S) ->
    #mekao_table{
        columns = MekaoCols,
        order_by = OrderBy
    } = Table,

    {NextNum, QData} = qdata(1, e2l(E), MekaoCols, S),
    {Where, {Types, Vals}} = where(QData, S),

    AllCols = mekao_utils:intersperse(
        MekaoCols, <<", ">>, fun(#mekao_column{name = Name}) -> Name end
    ),

    Q = #mekao_select{
        table       = Table#mekao_table.name,
        columns     = AllCols,
        where       = Where,
        order_by    = order_by(OrderBy)
    },
    limit(
        #mekao_query{
            body     = Q,
            types    = Types,
            values   = Vals,
            next_ph_num = NextNum
        }, Opts, S
    ).


-spec prepare_update(entity(), selector(), table(), s()) -> p_query().
prepare_update(SetE, WhereE, Table = #mekao_table{columns = MekaoCols}, S) ->
    {SetNextNum, {SetCols, SetPHs, SetTypes, SetVals}} =
        qdata(1, e2l(SetE), MekaoCols, S),

    {WhereNextNum, WhereQData} =
        qdata(SetNextNum, e2l(WhereE), MekaoCols, S),

    {Where, {WhereTypes, WhereVals}} =
        where(WhereQData, S),

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
        next_ph_num = WhereNextNum
    }.


-spec prepare_delete(selector(), table(), s()) -> p_query().
prepare_delete(E, Table, S) ->
    {NextNum, QData} =
        qdata(1, e2l(E), Table#mekao_table.columns, S),
    {Where, {Types, Vals}}
        = where(QData, S),

    Q = #mekao_delete{
        table       = Table#mekao_table.name,
        where       = Where,
        returning   = returning(delete, Table, S)
    },
    #mekao_query{
        body     = Q,
        types    = Types,
        values   = Vals,
        next_ph_num = NextNum
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
            untriplify(Columns, fun (C) -> [<<"SELECT ">>, C] end),
            <<" ">>,
            untriplify(Table, fun(C) -> [<<"FROM ">>, C] end),
            untriplify(Where, fun build_where/1),
            untriplify(OrderBy, fun build_order_by/1)
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
            <<"INSERT ">>, untriplify(Table, fun(C) -> [<<"INTO ">>, C] end),
            <<" ">>,
            untriplify(Columns, fun (Cs) -> [<<"(">>, Cs, <<")">>] end),
            <<" ">>,
            untriplify(Values, fun (Vs) -> [<<"VALUES ">>, Vs] end),
            untriplify(Return, fun build_return/1)
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
            untriplify(Table, fun (C) -> [<<"UPDATE ">>, C] end),
            <<" ">>,
            untriplify(Set, fun (C) -> [<<"SET ">>, C] end),
            untriplify(Where, fun build_where/1),
            untriplify(Return, fun build_return/1)
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
            <<"DELETE ">>,
            untriplify(Table, fun(C) -> [<<"FROM ">>, C] end),
            untriplify(Where, fun build_where/1),
            untriplify(Return, fun build_return/1)
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


qdata(Num, [], [], _) ->
    {Num, {[], [], [], []}};

qdata(Num, ['$skip' | Vals], [_Col | Cols], S) ->
    qdata(Num, Vals, Cols, S);

qdata(Num, [Pred | Vals], [#mekao_column{name = CName} = Col | Cols], S) ->
    {NextNum, NewPH, NewT, NewV} = qdata_predicate(Num, Pred, Col, S),

    {ResNum, {ResCols, ResPHs, ResTypes, ResVals}} = qdata(
        NextNum, Vals, Cols, S
    ),

    {ResNum, {[CName | ResCols], [NewPH | ResPHs], [NewT | ResTypes],
        [NewV | ResVals]}}.


qdata_insert(Num, [], [], _) ->
    {Num, {[], [], []}};

qdata_insert(Num, ['$skip' | Vals], [_Col | Cols], S) ->
    {ResNum, {ResPHs, ResTypes, ResVals}} = qdata_insert(
        Num, Vals, Cols, S
    ),
    {ResNum, {[<<"DEFAULT">> | ResPHs], ResTypes, ResVals}};

qdata_insert(Num, [Pred | Vals], [Col | Cols], S) ->
    {NextNum, NewPH, NewT, NewV} = qdata_plain_predicate(Num, Pred, Col, S),

    {ResNum, {ResPHs, ResTypes, ResVals}} = qdata_insert(
        NextNum, Vals, Cols, S
    ),

    {ResNum, {[NewPH | ResPHs], [NewT | ResTypes], [NewV | ResVals]}}.


qdata_predicate(Num, {'$predicate', 'not', Pred}, Col, S) ->
    {NextNum, NewPH, NewT, NewV} = qdata_predicate(Num, Pred, Col, S),
    {NextNum, NewPH, NewT, {'$predicate', 'not', NewV}};

qdata_predicate(Num, {'$predicate', in, InVals}, Col, S) ->
    #mekao_settings{placeholder = PHFun} = S,
    #mekao_column{type = T, transform = TrFun} = Col,

    %% intentional `error:badmatch' to prevent empty `... IN ()'
    true = is_list(InVals) andalso InVals /= [],
    {NewNum, RevPHs, RevTypes, RevVals} =
        lists:foldl(
            fun(InV, {InNum, InPHs, InTypes, InTransVals}) ->
                TransV = transform(TrFun, InV),
                PH = PHFun(Col, InNum, TransV),
                {InNum + 1, [PH | InPHs], [T | InTypes],
                    [TransV | InTransVals]}
            end, {Num, [], [], []}, InVals
        ),
    {NewNum, lists:reverse(RevPHs), lists:reverse(RevTypes),
        {'$predicate', in, lists:reverse(RevVals)}};

qdata_predicate(Num, {'$predicate', 'between', V1, V2}, Col, S) ->
    #mekao_settings{placeholder = PHFun} = S,
    #mekao_column{type = T, transform = TrFun} = Col,

    TransV1 = transform(TrFun, V1),
    TransV2 = transform(TrFun, V2),
    PH1 = PHFun(Col, Num, TransV1),
    PH2 = PHFun(Col, Num + 1, TransV2),
    {Num + 2, {PH1, PH2}, T,
        {'$predicate', 'between', TransV1, TransV2}
    };

qdata_predicate(Num, {'$predicate', Op, V}, Col, S) ->
    #mekao_settings{placeholder = PHFun} = S,
    #mekao_column{type = T, transform = TrFun} = Col,

    TransV = transform(TrFun, V),
    PH = PHFun(Col, Num, TransV),
    {Num + 1, PH, T, {'$predicate', Op, TransV}};

qdata_predicate(Num, V, Col, S) ->
    qdata_plain_predicate(Num, V, Col, S).


qdata_plain_predicate(Num, V, Col, S) ->
    #mekao_settings{placeholder = PHFun} = S,
    #mekao_column{type = T, transform = TrFun} = Col,

    TransV = transform(TrFun, V),
    PH = PHFun(Col, Num, TransV),
    {Num + 1, PH, T, TransV}.


-spec returning(insert | update | delete, table(), s()) -> iolist().
returning(_QType, _Table, #mekao_settings{returning = undefined}) ->
    [];
returning(QType, Table, #mekao_settings{returning = RetFun}) ->
    RetFun(QType, Table).


where({[], [], [], []}, _S) ->
    {[], {[], []}};

where({[C], [PH], [T], [V]}, S) ->
    {W, {NewTs, NewVs}} = predicate({C, PH, T, V}, S),
    {[W], {NewTs, NewVs}};

where({[C | Cs], [PH | PHs], [T | Types], [V | Vals]}, S) ->
    {W, {NewTs, NewVs}} = predicate({C, PH, T, V}, S),
    {Ws, {ResTs, ResVs}} = where({Cs, PHs, Types, Vals}, S),
    {[W, <<" AND ">> | Ws],
        {NewTs ++ ResTs, NewVs ++ ResVs}}.


limit( PSelect, Opts, #mekao_settings{limit = undefined}) ->
    %% intentional error:badmatch in case if `limit' was specified but
    %% `#mekao_settings.limit' was not
    undefined = proplists:get_value(limit, Opts),
    PSelect;
limit( PSelect, Opts, #mekao_settings{limit = LimitFun}
     ) when is_function(LimitFun, 3) ->
    case proplists:get_value(limit, Opts) of
        undefined ->
            PSelect;
        {RowCount, Offset} ->
            #mekao_query{} = LimitFun(PSelect, RowCount, Offset)
    end.


%% TODO: add ANY, ALL handling
predicate({C, PH, T, {'$predicate', Op, V}}, S) when Op == '='; Op == '<>' ->
    IsNull = (S#mekao_settings.is_null)(V),
    if not IsNull ->
        {[C, op_to_bin(Op), PH], {[T], [V]}};
    Op == '=' ->
        {[C, <<" IS NULL">>], {[], []}};
    Op == '<>' ->
        {[C, <<" IS NOT NULL">>], {[], []}}
    end;

predicate({C, PH, T, {'$predicate', 'not', Pred}}, _S) ->
    {W, Rest} = predicate({C, PH, T, Pred}, _S),
    {[<<"NOT (">>, W, <<")">>], Rest};

predicate({C, {PH1, PH2}, T, {'$predicate', between, V1, V2}}, _S) ->
    {[C, <<" BETWEEN ">>, PH1, <<" AND ">>, PH2], {[T, T], [V1, V2]}};

predicate({C, PH, T, {'$predicate', like, V}}, _S) ->
    {[C, <<" LIKE ">>, PH], {[T], [V]}};

predicate( {C, PHs, Ts, {'$predicate', in, Vals}}, _S
         ) when is_list(Vals), Vals /= [] ->
    {[C, <<" IN (">>,
            mekao_utils:intersperse(PHs, <<", ">>),
        <<")">>
    ], {Ts, Vals}};

predicate({C, PH, T, {'$predicate', OP, V}}, _S) ->
    {[C, op_to_bin(OP), PH], {[T], [V]}};

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

untriplify({C1, C2, C3}, F) when is_function(F) ->
    [C1, F(C2), C3];
untriplify(C, F) ->
    F(C).

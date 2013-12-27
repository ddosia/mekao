-module(mekao_tests).

-record(book, {id, isbn, title, author, created}).

-include("mekao.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TABLE_BOOKS, #mekao_table{
    name    =  <<"books">>,
    columns = [
        #mekao_column{name = <<"id">>, type = int, key = true, ro = true},
        #mekao_column{name = <<"isbn">>, type = varchar},
        #mekao_column{name = <<"title">>, type = varchar},
        #mekao_column{name = <<"author">>, type = varchar},
        #mekao_column{name = <<"created">>, type = varchar, ro = true}
    ]
}).

-define(S, #mekao_settings{
    placeholder = fun (Pos, _) -> [$$ | integer_to_list(Pos)] end,
    is_null     = fun(V) -> V == undefined end
}).

-define(MK_CALL(CallName, E, Table, S), begin
    {Q, Types, Vals} = mekao:CallName(E, Table, S),
    {iolist_to_binary(Q), Types, Vals}
end).

-define(MK_CALL(CallName, E, Table), ?MK_CALL(CallName, E, Table, ?S)).

-define(MK_CALL(CallName, E), ?MK_CALL(CallName, E, ?TABLE_BOOKS)).

%%%===================================================================
%%% Tests
%%%===================================================================

is_null_test_() ->
    Null = erlang:make_ref(), [
        %% TODO: extend this to test UPDATE and DELETE
        ?_assertEqual(
            {<<"SELECT id, isbn, title, author, created FROM books",
               " WHERE id IS NULL;">>, [int], [Null]},
            ?MK_CALL(select_pk, book(Null), ?TABLE_BOOKS, ?S#mekao_settings{
                is_null = fun(V) -> V == Null end
            })
        )
    ].


empty_where_test_() -> [
    %% TODO: extend this to test UPDATE and DELETE
    ?_assertEqual(
        {<<"SELECT id, isbn, title, author, created FROM books;">>, [], []},
        ?MK_CALL(select, #book{_ = '$skip'})
    )
].


skip_test_() ->
    %% TODO: extend this to test UPDATE and DELETE
    #book{author = Author, title = Title} = book(undefined), [
        ?_assertEqual(
            {<<"SELECT id, isbn, title, author, created FROM books",
                " WHERE title = $1 AND author = $2;">>,
                [varchar, varchar], [Title, Author]},
            ?MK_CALL(
                select, #book{author = Author, title = Title, _ = '$skip'}
            )
        ),
        ?_assertEqual(
            {<<"INSERT INTO books (title, author) VALUES ($1, $2);">>,
                [varchar, varchar], [Title, Author]},
            ?MK_CALL(
                insert, #book{author = Author, title = Title, _ = '$skip'}
            )
        )
    ].


select_pk_test() ->
    ?assertEqual(
        {<<"SELECT id, isbn, title, author, created FROM books",
            " WHERE id = $1;">>, [int], [1]},
        ?MK_CALL(select_pk, book(1))
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

book(Id) ->
    #book{
        id      = Id,
        isbn    = <<"978-1593274351">>,
        title   = <<"Learn You Some Erlang for Great Good!: A Beginner's Guide">>,
        author  = <<"Fred Hebert">>,
        created = calendar:now_to_datetime(os:timestamp())
    }.

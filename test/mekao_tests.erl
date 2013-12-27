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

%%%===================================================================
%%% Tests
%%%===================================================================

is_null_test_() ->
    Null = erlang:make_ref(), [
        %% TODO: extend this to test UPDATE and DELETE
        ?_assertEqual(
            {<<"SELECT id, isbn, title, author, created FROM books",
               " WHERE id IS NULL;">>, [int], [Null]},
            mk_call(select_pk, book(Null), ?TABLE_BOOKS, ?S#mekao_settings{
                is_null = fun(V) -> V == Null end
            })
        )
    ].


empty_where_test_() -> [
    %% TODO: extend this to test UPDATE and DELETE
    ?_assertEqual(
        {<<"SELECT id, isbn, title, author, created FROM books;">>, [], []},
        mk_call(select, #book{_ = '$skip'})
    )
].


skip_test_() ->
    %% TODO: extend this to test UPDATE and DELETE
    #book{author = Author, title = Title} = book(undefined), [
        ?_assertEqual(
            {<<"SELECT id, isbn, title, author, created FROM books",
                " WHERE title = $1 AND author = $2;">>,
                [varchar, varchar], [Title, Author]},
            mk_call(
                select, #book{author = Author, title = Title, _ = '$skip'}
            )
        ),
        ?_assertEqual(
            {<<"INSERT INTO books (title, author) VALUES ($1, $2);">>,
                [varchar, varchar], [Title, Author]},
            mk_call(
                insert, #book{author = Author, title = Title, _ = '$skip'}
            )
        )
    ].


returning_test_() ->
    RetFun =
        fun
            (T, #mekao_table{columns = Cols}) when T == insert; T == update ->
                CNames = mekao_utils:intersperse(
                    [CName || #mekao_column{name = CName} <- Cols],
                    <<", ">>
                ),
                [<<"RETURNING ">>, CNames];
            (delete, #mekao_table{columns = Cols}) ->
                CNames = mekao_utils:intersperse(
                    [CName || #mekao_column{name = CName, key = true} <- Cols],
                    <<", ">>
                ),
                [<<"RETURNING ">>, CNames]
        end,
    Book = #book{isbn = Isbn, title = Title, author = Author} = book(1),
    [
        ?_assertEqual(
            {<<"INSERT INTO books (isbn, title, author)",
                " VALUES ($1, $2, $3)"
                " RETURNING id, isbn, title, author, created;">>,
                [varchar, varchar, varchar], [Isbn, Title, Author]},
            mk_call(insert, book(1), ?TABLE_BOOKS, ?S#mekao_settings{
                returning = RetFun
            })
        ),
        ?_assertEqual(
            {<<"UPDATE books SET isbn = $1, title = $2, author = $3",
                " WHERE id = $4",
                " RETURNING id, isbn, title, author, created;">>,
                [varchar, varchar, varchar, int], [Isbn, Title, Author, 1]},
            mk_call(update_pk, book(1), ?TABLE_BOOKS, ?S#mekao_settings{
                returning = RetFun
            })
        ),
        ?_assertEqual(
            {<<"DELETE FROM books WHERE id = $1 RETURNING id;">>, [int], [1]},
            mk_call(delete_pk, book(1), ?TABLE_BOOKS, ?S#mekao_settings{
                returning = RetFun
            })
        )
    ].


select_pk_test() ->
    ?assertEqual(
        {<<"SELECT id, isbn, title, author, created FROM books",
            " WHERE id = $1;">>, [int], [1]},
        mk_call(select_pk, book(1))
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

mk_call(CallName, E) ->
    mk_call(CallName, E, ?TABLE_BOOKS).

mk_call(CallName, E, Table) ->
    mk_call(CallName, E, Table, ?S).

mk_call(CallName, E, Table, S) ->
    {Q, Types, Vals} = mekao:CallName(E, Table, S),
    {iolist_to_binary(Q), Types, Vals}.

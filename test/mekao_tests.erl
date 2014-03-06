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
        #mekao_column{name = <<"created">>, type = datetime, ro = true}
    ]
}).

-define(S, #mekao_settings{
    placeholder = fun (_, Pos, _) -> [$$ | integer_to_list(Pos)] end
}).

%%%===================================================================
%%% Tests
%%%===================================================================

is_null_test_() ->
    Null = erlang:make_ref(),
    S = ?S#mekao_settings{is_null = fun(V) -> V == Null end},
    [
        %% TODO: extend this to test UPDATE and DELETE
        ?_assertMatch(
            #mekao_query{
                body = <<"SELECT id, isbn, title, author, created FROM books",
                        " WHERE author IS NULL">>,
                types = [varchar],
                values = [Null]
            },
            mk_call(
                select, #book{author = Null, _ = '$skip'}, ?TABLE_BOOKS, S
            )
        ),
        ?_assertMatch(
            #mekao_query{
                body = <<"SELECT id, isbn, title, author, created FROM books",
                        " WHERE author IS NOT NULL">>,
                types = [varchar],
                values = [Null]
            },
            mk_call(
                select, #book{author = {'$predicate', '<>', Null}, _ = '$skip'},
                ?TABLE_BOOKS, S
            )
        )
    ].


empty_where_test_() -> [
    %% TODO: extend this to test UPDATE and DELETE
    ?_assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books">>,
            types = [],
            values = []
        },
        mk_call(select, #book{_ = '$skip'})
    )
].


skip_test_() ->
    %% TODO: extend this to test UPDATE and DELETE
    #book{author = Author, title = Title} = book(undefined), [
        ?_assertMatch(
            #mekao_query{
                body = <<"SELECT id, isbn, title, author, created FROM books",
                        " WHERE title = $1 AND author = $2">>,
                types = [varchar, varchar],
                values = [Title, Author]
            },
            mk_call(
                select, #book{author = Author, title = Title, _ = '$skip'}
            )
        ),
        ?_assertMatch(
            #mekao_query{
                body = <<"INSERT INTO books (title, author) VALUES ($1, $2)">>,
                types = [varchar, varchar],
                values = [Title, Author]
            },
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
    #book{isbn = Isbn, title = Title, author = Author} = book(1),
    [
        ?_assertMatch(
            #mekao_query{
                body = <<"INSERT INTO books (isbn, title, author)",
                        " VALUES ($1, $2, $3)"
                        " RETURNING id, isbn, title, author, created">>,
                types = [varchar, varchar, varchar],
                values = [Isbn, Title, Author]
            },
            mk_call(insert, book(1), ?TABLE_BOOKS, ?S#mekao_settings{
                returning = RetFun
            })
        ),
        ?_assertMatch(
            #mekao_query{
                body = <<"UPDATE books SET isbn = $1, title = $2, author = $3",
                        " WHERE id = $4",
                        " RETURNING id, isbn, title, author, created">>,
                types = [varchar, varchar, varchar, int],
                values = [Isbn, Title, Author, 1]
            },
            mk_call(update_pk, book(1), ?TABLE_BOOKS, ?S#mekao_settings{
                returning = RetFun
            })
        ),
        ?_assertMatch(
            #mekao_query{
                body = <<"DELETE FROM books WHERE id = $1 RETURNING id">>,
                types = [int],
                values = [1]
            },
            mk_call(delete_pk, book(1), ?TABLE_BOOKS, ?S#mekao_settings{
                returning = RetFun
            })
        )
    ].

column_type_test() ->
    Table0 = #mekao_table{columns = Cols} = ?TABLE_BOOKS,

    Table = Table0#mekao_table{
        columns = Cols ++ [
            #mekao_column{
                name = <<"type">>, type = int,
                transform = fun (undefined) -> 0;
                                (hardcover) -> 1;
                                (paperback) -> 2
                            end
            }
        ]
    },
    Book = #book{isbn = Isbn, title = Title, author = Author} = book(1),
    ?assertMatch(
        #mekao_query{
            body = <<"INSERT INTO books (isbn, title, author, type)",
                    " VALUES ($1, $2, $3, $4)">>,
            types = [varchar, varchar, varchar, int],
            values = [Isbn, Title, Author, 2]
        },
        mk_call(insert, erlang:append_element(Book, paperback), Table, ?S)
    ).

placeholder_test() ->
    S = ?S#mekao_settings{
        placeholder =
            fun
                (#mekao_column{type = int}, _Pos, Val) ->
                    integer_to_list(Val);
                (#mekao_column{type = varchar}, _Pos, Val) ->
                    [$', Val, $']
            end
    },

    Table = #mekao_table{columns = Cols} = ?TABLE_BOOKS,
    AuthorCol = lists:keyfind(<<"author">>, #mekao_column.name, Cols),
    NewAuthorCol = AuthorCol#mekao_column{
        transform =
            fun
                (undefined) -> <<"">>;
                (Val) -> Val
            end
    },
    NewCols = lists:keyreplace(
        <<"author">>, #mekao_column.name, Cols, NewAuthorCol
    ),

    Book = #book{isbn = Isbn, title = Title} = book(1),

    QBody = <<"UPDATE books SET isbn = '", Isbn/binary, "',",
            " title = '", Title/binary, "', author = '' WHERE id = 1">>,

    ?assertMatch(
        #mekao_query{
            body = QBody,
            types = [varchar, varchar, varchar, int],
            values = [Isbn, Title, <<"">>, 1]
        },
        mk_call(
            update_pk, Book#book{author = undefined},
            Table#mekao_table{columns = NewCols}, S
        )
    ).


predicate_test_() ->
    Ops = ['=', '<>', '>', '>=', '<', '<='],
    DT = {{2013, 1, 1}, {0, 0, 0}},
    lists:map(
        fun(Op) ->
            OpBin = atom_to_binary(Op, utf8),
            QBody = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE created ", OpBin/binary," $1">>,
            ?_assertMatch(
                #mekao_query{
                    body = QBody, types = [datetime], values = [DT]
                },
                mk_call(
                    select, #book{created = {'$predicate', Op, DT}, _ = '$skip'}
                )
            )
        end, Ops
    ).


prepare_select1_test() ->
    #book{author = Author} = book(1),
    Q = #mekao_query{body = QBody = #mekao_select{where = Where}} =
        mekao:prepare_select(
            #book{author = Author, _ = '$skip'}, ?TABLE_BOOKS, ?S
        ),

    NewQ = #mekao_query{body = NewQBody} =
        mekao:build(Q#mekao_query{
            body = QBody#mekao_select{
                where = [Where, <<" AND created >= now() - interval '7 day'">>]
            }
        }),

    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE author = $1 AND created >= now() - interval",
                    " '7 day'">>,
            types = [varchar],
            values = [Author]
        },
        NewQ#mekao_query{body = iolist_to_binary(NewQBody)}
    ).

prepare_select2_test() ->
    #book{author = Author} = book(1),
    Author2 = <<"Francesco Cesarini">>,

    Q = #mekao_query{
        body = QBody = #mekao_select{where = Where},
        types = Types,
        values = Vals,
        next_ph_num = Num
    } = mekao:prepare_select(
        #book{author = Author, _ = '$skip'}, ?TABLE_BOOKS, ?S
    ),

    NewQ = #mekao_query{body = NewQBody} =
        mekao:build(Q#mekao_query{
            body = QBody#mekao_select{
                where = [Where, <<" OR author = $">>, integer_to_list(Num)]
            }
        }),

    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE author = $1 OR author = $2">>,
            types = [varchar, int],
            values = [Author, Author2]
        },
        NewQ#mekao_query{
            body = iolist_to_binary(NewQBody),
            types = Types ++ [int],
            values = Vals ++ [Author2]
        }
    ).


select_pk_test() ->
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE id = $1">>,
            types = [int],
            values = [1]
        },
        mk_call(select_pk, book(1))
    ).


order_by_test() ->
    T = ?TABLE_BOOKS#mekao_table{
        order_by = [
            #book.author, {#book.title, {desc, default}}, "EXTRACT(DAY FROM created)"
        ]
    },
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " ORDER BY 4, 3 DESC, EXTRACT(DAY FROM created)">>
        },
        mk_call(
            select, #book{_ = '$skip'}, T, ?S
        )
    ).


insert_test() ->
    Book = #book{isbn = Isbn, title = Title, author = Author} = book(1),
    ?assertMatch(
        #mekao_query{
            body = <<"INSERT INTO books (isbn, title, author)",
                    " VALUES ($1, $2, $3)">>,
            types = [varchar, varchar, varchar],
            values = [Isbn, Title, Author]},
        mk_call(insert, Book)
    ).


update_test() ->
    #book{isbn = Isbn, title = Title} = book(1),
    {ok, UpdateQ = #mekao_query{body = UpdateQBody}}
        = mekao:update(
            #book{title = Title, _ = '$skip'}, #book{isbn = Isbn, _ = '$skip'},
            ?TABLE_BOOKS, ?S
        ),

    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET title = $1 WHERE isbn = $2">>,
            types = [varchar, varchar],
            values = [Title, Isbn]
        },
        UpdateQ#mekao_query{
            body = iolist_to_binary(UpdateQBody)
        }
    ).


update_pk_test() ->
    Book = #book{isbn = Isbn, title = Title, author = Author} = book(1),
    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET isbn = $1, title = $2, author = $3",
                    " WHERE id = $4">>,
            types = [varchar, varchar, varchar, int],
            values = [Isbn, Title, Author, 1]
        },
        mk_call(update_pk, Book)
    ).


update_pk_diff_test() ->
    Book = #book{title = Title} = book(1),

    {ok, UpdatePKDiffQ = #mekao_query{body = UpdatePKDiffQBody}}
        = mekao:update_pk_diff(
            Book#book{title = <<"Unknown">>}, Book, ?TABLE_BOOKS, ?S
        ),

    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET title = $1 WHERE id = $2">>,
            types = [varchar, int],
            values = [Title, 1]
        },
        UpdatePKDiffQ#mekao_query{
            body = iolist_to_binary(UpdatePKDiffQBody)
        }
    ).


%% @doc When key is not read only, it must be changeable.
update_pk_diff_key_changed_test() ->
    TBooks = #mekao_table{columns = Cols} = ?TABLE_BOOKS,
    IdCol = #mekao_column{key = true, ro = true}
        = lists:keyfind(<<"id">>, #mekao_column.name, Cols),
    NewCols = lists:keystore(
        <<"id">>, #mekao_column.name, Cols, IdCol#mekao_column{ro = false}
    ),

    Book = #book{title = Title} = book(2),

    {ok, UpdatePKDiffQ = #mekao_query{body = UpdatePKDiffQBody}}
        = mekao:update_pk_diff(
            Book#book{id = 1, title = <<"Unknown">>}, Book,
            TBooks#mekao_table{columns = NewCols}, ?S
        ),

    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET id = $1, title = $2 WHERE id = $3">>,
            types = [int, varchar, int],
            values = [2, Title, 1]
        },
        UpdatePKDiffQ#mekao_query{
            body = iolist_to_binary(UpdatePKDiffQBody)
        }
    ).


delete_pk_test() ->
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE id = $1">>,
            types = [int],
            values = [1]
        },
        mk_call(delete_pk, book(1))
    ).

delete_test() ->
    #book{author = Author} = book(1),
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE author = $1">>,
            types = [varchar],
            values = [Author]
        },
        mk_call(delete, #book{author = Author, _ = '$skip'})
    ).

error_test_() -> [
    ?_assertMatch(
        {error, empty_insert},
        mekao:insert(#book{_ = '$skip'}, ?TABLE_BOOKS, ?S)
    ),
    ?_assertMatch(
        {error, pk_miss},
        mekao:select_pk(book('$skip'), ?TABLE_BOOKS, ?S)
    ),
    ?_assertMatch(
        {error, pk_miss},
        mekao:update_pk(book('$skip'), ?TABLE_BOOKS, ?S)
    ),
    ?_assertMatch(
        {error, empty_update},
        mekao:update_pk(#book{id = 1,_ = '$skip'}, ?TABLE_BOOKS, ?S)
    ),
    ?_assertMatch(
        {error, empty_update},
        mekao:update_pk_diff(book(1), book(1), ?TABLE_BOOKS, ?S)
    ),
    ?_assertMatch(
        {error, pk_miss},
        mekao:update_pk_diff(
            (book('$skip'))#book{isbn = <<"Unknown">>}, book('$skip'),
            ?TABLE_BOOKS, ?S
        )
    ),
    ?_assertMatch(
        {error, pk_miss},
        mekao:delete_pk(book('$skip'), ?TABLE_BOOKS, ?S)
    )
].


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
    {ok, Q = #mekao_query{body = B}} = mekao:CallName(E, Table, S),
    Q#mekao_query{body = iolist_to_binary(B)}.

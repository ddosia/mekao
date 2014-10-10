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
        #mekao_column{name = <<"created">>, type = timestamp, ro = true}
    ]
}).

-define(S, #mekao_settings{
    placeholder = fun (_, Pos, _) -> mk_ph(Pos) end
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
                types = [],
                values = []
            },
            mk_call(
                select, #book{author = Null, _ = '$skip'}, ?TABLE_BOOKS, S
            )
        ),
        ?_assertMatch(
            #mekao_query{
                body = <<"SELECT id, isbn, title, author, created FROM books",
                        " WHERE author IS NOT NULL">>,
                types = [],
                values = []
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


limit_test() ->
    RowCount0 = 10,
    Offset0   = 40,
    S = ?S#mekao_settings{
        limit = fun
            (Q, RowCount, Offset) ->
                #mekao_query{
                    body = #mekao_select{where = Where} = QBody,
                    types = Types, values = Vals, next_ph_num = Num
                } = Q,
                Q#mekao_query{
                    body = QBody#mekao_select{
                        where = {<<"">>, Where, [
                            <<" LIMIT ">>, mk_ph(Num),
                            <<" OFFSET ">>, mk_ph(Num + 1)
                        ]}
                    },
                    types = Types ++ [int, int],
                    values = Vals ++ [RowCount, Offset],
                    next_ph_num = Num + 2
                }
        end
    },
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " LIMIT $1 OFFSET $2">>,
            types  = [int, int],
            values = [RowCount0, Offset0],
            next_ph_num = 3
        },
        mk_call(
            select,
            #book{_ = '$skip' },
            [{limit, {RowCount0, Offset0}}],
            ?TABLE_BOOKS, S
        )
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE title LIKE $1 LIMIT $2 OFFSET $3">>,
            types  = [varchar, int, int],
            values = [<<"%Erlang%">>, RowCount0, Offset0],
            next_ph_num = 4
        },
        mk_call(
            select,
            #book{
                title = {'$predicate', like, <<"%Erlang%">>},
                _ = '$skip'
            },
            [{limit, {RowCount0, Offset0}}],
            ?TABLE_BOOKS, S
        )
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books">>,
            types  = [], values = [], next_ph_num = 1
        },
        mk_call(select, #book{_ = '$skip'}, [], ?TABLE_BOOKS, S)
    ),
    ?assertException(
        error, {badmatch, {RowCount0, Offset0}},
        mk_call(
            select,
            #book{_ = '$skip'},
            [{limit, {RowCount0, Offset0}}],
            ?TABLE_BOOKS, ?S
        )
    ).


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
                    body = QBody, types = [timestamp], values = [DT]
                },
                mk_call(
                    select, #book{created = {'$predicate', Op, DT}, _ = '$skip'}
                )
            )
        end, Ops
    ).


like_test() ->
    Title = <<"%Erlang%">>,
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE title LIKE $1">>,
            types = [varchar],
            values = [Title]
        },
        mk_call(
            select, #book{title = {'$predicate', like, Title}, _ = '$skip'}
        )
    ).


between_test() ->
    NowSecs = calendar:datetime_to_gregorian_seconds(
        calendar:now_to_datetime(os:timestamp())
    ),
    DT1 = calendar:gregorian_seconds_to_datetime(NowSecs - 24 * 60 * 60),
    DT2 = calendar:gregorian_seconds_to_datetime(NowSecs),

    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE created BETWEEN $1 AND $2">>,
            types  = [timestamp, timestamp],
            values = [DT1, DT2],
            next_ph_num = 3
        },
        mk_call(
            select, #book{
                created = {'$predicate', between, DT1, DT2},
                _ = '$skip'
            }
        )
    ).

in_test() ->
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE id IN ($1, $2, $3, $4)">>,
            types = [int, int, int, int],
            values = [1, 2, 3, 4]
        },
        mk_call(
            select, #book{id = {'$predicate', in, [1, 2, 3, 4]}, _ = '$skip'}
        )
    ),
    {ok, UpdateQ = #mekao_query{body = UpdateQBody}}
        = mekao:update(
            #book{title = NewTitle = <<"NewTitle">>, _ = '$skip'},
            #book{id = {'$predicate', in, [1, 2, 3, 4]}, _ = '$skip'},
            ?TABLE_BOOKS, ?S
        ),
    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET title = $1"
                    " WHERE id IN ($2, $3, $4, $5)">>,
            types = [varchar, int, int, int, int],
            values = [NewTitle, 1, 2, 3, 4]
        },
        UpdateQ#mekao_query{
            body = iolist_to_binary(UpdateQBody)
        }
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE id IN ($1, $2, $3, $4)">>,
            types = [int, int, int, int],
            values = [1, 2, 3, 4]
        },
        mk_call(
            delete,
            #book{id = {'$predicate', in, [1, 2, 3, 4]}, _ = '$skip'}
        )
    ),
    ?assertException(
        error, {badmatch, false},
        mk_call(delete, #book{id = {'$predicate', in, []}, _ = '$skip'})
    ).


not_test() ->
    Title = <<"Erlang">>,
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE NOT (title = $1)">>,
            types = [varchar],
            values = [Title]
        },
        mk_call(
            delete,
            #book{title = {'$predicate', 'not', Title}, _ = '$skip'}
        )
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books"
                    " WHERE NOT (title IS NULL)">>,
            types = [],
            values = []
        },
        mk_call(
            select,
            #book{title = {'$predicate', 'not', undefined}, _ = '$skip'},
            ?TABLE_BOOKS,
            ?S#mekao_settings{is_null = fun(V) -> V == undefined end}
        )
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE NOT (title LIKE $1)">>,
            types = [varchar],
            values = [Title]
        },
        mk_call(
            delete,
            #book{
                title = {'$predicate', 'not',
                    {'$predicate', like, Title}
                }, _ = '$skip'
            }
        )
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE NOT (NOT (title = $1))">>,
            types = [varchar],
            values = [Title]
        },
        mk_call(
            delete,
            #book{
                title = {'$predicate', 'not',
                    {'$predicate', 'not', Title}
                }, _ = '$skip'
            }
        )
    ).


prepare_select_test() ->
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
                where = [Where, <<" OR author = ">>, mk_ph(Num)]
            },
            types = Types ++ [int],
            values = Vals ++ [Author2],
            next_ph_num = Num + 1
        }),

    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE author = $1 OR author = $2">>,
            types = [varchar, int],
            values = [Author, Author2],
            next_ph_num = 3
        },
        NewQ#mekao_query{body = iolist_to_binary(NewQBody)}
    ).


exists_test() ->
    #book{id = Id, isbn = Isbn, author = Author} = book(1),
    ?assertMatch(
        #mekao_query{
            body = <<
                "SELECT COUNT(*) AS exists FROM (SELECT 1) AS t WHERE EXISTS("
                    "SELECT 1 FROM books WHERE isbn = $1 AND author = $2"
                ")"
            >>, types = [varchar, varchar], values = [Isbn, Author]
        },
        mk_call(
            exists,
            #book{isbn = Isbn, author = Author, _ = '$skip'}
        )
    ),
    ?assertMatch(
        #mekao_query{
            body = <<
                "SELECT COUNT(*) AS exists FROM (SELECT 1) AS t WHERE EXISTS("
                    "SELECT 1 FROM books WHERE id IS NULL"
                ")"
            >>, types = [], values = []
        },
        mk_call(
            exists_pk,
            #book{id = undefined, _ = '$skip'}
        )
    ),
    ?assertMatch(
        #mekao_query{
            body = <<
                "SELECT COUNT(*) AS exists FROM (SELECT 1) AS t WHERE EXISTS("
                    "SELECT 1 FROM books WHERE id = $1"
                ")"
            >>, types = [int], values = [Id]
        },
        mk_call(
            exists_pk,
            #book{id = Id, author = <<"Doesn't matter">>, _ = '$skip'}
        )
    ),
    ?assertMatch(
        {error, pk_miss},
        mekao:exists_pk(book('$skip'), ?TABLE_BOOKS, ?S)
    ).


count_test() ->
    #book{isbn = Isbn, author = Author} = book(1),
    ?assertMatch(
        #mekao_query{
            body = <<
                "SELECT COUNT(*) as count FROM books"
                " WHERE isbn = $1 AND author = $2"
            >>, types = [varchar, varchar], values = [Isbn, Author]
        },
        mk_call(
            count,
            #book{isbn = Isbn, author = Author, _ = '$skip'}
        )
    ).


prepare_select_triple_test() ->
    T = ?TABLE_BOOKS#mekao_table{order_by = [#book.title]},
    #book{author = Author} = book(1),

    Q = #mekao_query{
        body = QBody = #mekao_select{
            columns  = Cols,
            table    = Table,
            where    = Where,
            order_by = OrderBy
        }
    } = mekao:prepare_select(
        #book{author = Author, _ = '$skip'}, T, ?S
    ),

    NewQ = #mekao_query{body = NewQBody} =
        mekao:build(Q#mekao_query{
            body = QBody#mekao_select{
                columns  = {<<"|1|">>, Cols,    <<"|2|">>},
                table    = {<<"|3|">>, Table,   <<"|4|">>},
                where    = {<<"|5|">>, Where,   <<"|6|">>},
                order_by = {<<"|7|">>, OrderBy, <<"|8|">>}
            }
        }),
    ?assertMatch(
        #mekao_query{
            body = <<"|1|SELECT id, isbn, title, author, created|2|",
                    " |3|FROM books|4||5| WHERE author = $1|6||7|",
                    " ORDER BY 3|8|">>,
            types = [varchar],
            values = [Author]
        },
        NewQ#mekao_query{body = iolist_to_binary(NewQBody)}
    ).


prepare_insert_triple_test() ->
    Q = #mekao_query{
        body = QBody = #mekao_insert{
            table        = Table,
            columns      = Columns,
            values       = Values
        }
    } = mekao:prepare_insert(book(1), ?TABLE_BOOKS, ?S),

    NewQ = #mekao_query{body = NewQBody} =
        mekao:build(Q#mekao_query{
            body = QBody#mekao_insert{
                table     = {<<"|1|">>, Table,   <<"|2|">>},
                columns   = {<<"|3|">>, Columns, <<"|4|">>},
                values    = {<<"|5|">>, Values,  <<"|6|">>},
                returning = {<<"|7|">>, <<"RETURNING *">>, <<"|8|">>}
            }
        }),
    ?assertMatch(
        #mekao_query{
            body = <<"INSERT |1|INTO books|2|",
                    " |3|(id, isbn, title, author, created)|4|",
                    " |5|VALUES ($1, $2, $3, $4, $5)|6|",
                    "|7| RETURNING *|8|">>
        },
        NewQ#mekao_query{body = iolist_to_binary(NewQBody)}
    ).


prepare_update_triple_test() ->
    #book{id = Id, isbn = Isbn} = book(1),
    Q = #mekao_query{
        body = QBody = #mekao_update{
            table       = Table,
            set         = Set,
            where       = Where
        }
    } = mekao:prepare_update(
        #book{isbn = Isbn, _ = '$skip'},
        #book{id = Id, _ = '$skip'},
        ?TABLE_BOOKS, ?S
    ),

    NewQ = #mekao_query{body = NewQBody} =
        mekao:build(Q#mekao_query{
            body = QBody#mekao_update{
                table     = {<<"|1|">>, Table, <<"|2|">>},
                set       = {<<"|3|">>, Set,   <<"|4|">>},
                where     = {<<"|5|">>, Where, <<"|6|">>},
                returning = {<<"|7|">>, <<"RETURNING *">>, <<"|8|">>}
            }
        }),
    ?assertMatch(
        #mekao_query{
            body = <<"|1|UPDATE books|2| |3|SET isbn = $1|4|",
                    "|5| WHERE id = $2|6||7| RETURNING *|8|">>,
            types   = [varchar, int],
            values  = [Isbn, Id]
        },
        NewQ#mekao_query{body = iolist_to_binary(NewQBody)}
    ).


prepare_delete_triple_test() ->
    Q = #mekao_query{
        body = QBody = #mekao_delete{
            table = Table,
            where = Where
        }
    } = mekao:prepare_delete(#book{id = 1, _ = '$skip'}, ?TABLE_BOOKS, ?S),

    NewQ = #mekao_query{body = NewQBody} =
        mekao:build(Q#mekao_query{
            body = QBody#mekao_delete{
                table     = {<<"|1|">>, Table, <<"|2|">>},
                where     = {<<"|3|">>, Where, <<"|4|">>},
                returning = {<<"|5|">>, <<"RETURNING *">>, <<"|6|">>}
            }
        }),
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE |1|FROM books|2||3| WHERE id = $1|4||5|",
                    " RETURNING *|6|">>,
            types   = [int],
            values  = [1]
        },
        NewQ#mekao_query{body = iolist_to_binary(NewQBody)}
    ).


select_pk_test() ->
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE id IS NULL">>,
            types = [], values = []
        },
        mk_call(select_pk, book(undefined))
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " WHERE id = $1">>,
            types = [int], values = [1]
        },
        mk_call(select_pk, book(1))
    ).


order_by_test() ->
    T = ?TABLE_BOOKS#mekao_table{
        order_by = [
            #book.id,
            {#book.isbn, {asc, default}},
            {#book.title, {desc, default}},
            {#book.author, {asc, nulls_first}},
            {#book.created, {desc, nulls_last}},
            "EXTRACT(DAY FROM created)"
        ]
    },
    ?assertMatch(
        #mekao_query{
            body = <<"SELECT id, isbn, title, author, created FROM books",
                    " ORDER BY 1, 2 ASC, 3 DESC, 4 ASC NULLS FIRST,"
                    " 5 DESC NULLS LAST, EXTRACT(DAY FROM created)">>
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

insert_all_test() ->
    #book{isbn = Isbn, title = Title, author = Author} = book(1),
        R = mk_call(
            insert_all, [
                #book{isbn = Isbn, _ = '$skip'},
                #book{title = Title, _ = '$skip'},
                #book{author = Author, _ = '$skip'}
            ]
        ),
    ?assertMatch(
        #mekao_query{
            body = <<"INSERT INTO books (id, isbn, title, author, created)",
                    " VALUES "
                        "(DEFAULT, $1, DEFAULT, DEFAULT, DEFAULT), "
                        "(DEFAULT, DEFAULT, $2, DEFAULT, DEFAULT), "
                        "(DEFAULT, DEFAULT, DEFAULT, $3, DEFAULT)"
                   >>,
            types = [varchar, varchar, varchar],
            values = [Isbn, Title, Author]
        },
        R
    ).


update_test() ->
    #book{isbn = Isbn, title = Title} = book(1),
    {ok, UpdateQ = #mekao_query{body = UpdateQBody}}
        = mekao:update(
            #book{title = Title, _ = '$skip'}, #book{isbn = Isbn, _ = '$skip'},
            ?TABLE_BOOKS, ?S
        ),

    ?assertMatch(
        {error, empty_update},
        mekao:update(
            #book{_ = '$skip'}, #book{isbn = Isbn, _ = '$skip'},
            ?TABLE_BOOKS, ?S
        )
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
                    " WHERE id IS NULL">>,
            types = [varchar, varchar, varchar],
            values = [Isbn, Title, Author]
        },
        mk_call(update_pk, Book#book{id = undefined})
    ),
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
update_key_changed_test() ->
    TBooks = #mekao_table{columns = Cols} = ?TABLE_BOOKS,
    IdCol = #mekao_column{key = true, ro = true}
        = lists:keyfind(<<"id">>, #mekao_column.name, Cols),
    NewCols = lists:keystore(
        <<"id">>, #mekao_column.name, Cols, IdCol#mekao_column{ro = false}
    ),
    NewTBooks = TBooks#mekao_table{columns = NewCols},

    Title = <<"NewTitle">>,

    {ok, Q1 = #mekao_query{body = QBody1}}
        = mekao:update(
            #book{id = 2, title = Title, _ = '$skip'},
            #book{id = 1, _ = '$skip'},
            NewTBooks,
            ?S
        ),


    {ok, Q2 = #mekao_query{body = QBody2}}
        = mekao:update_pk(
            #book{id = 1, title = Title, _ = '$skip'},
            NewTBooks,
            ?S
        ),

    Book = book(1),
    {ok, Q3 = #mekao_query{body = QBody3}}
        = mekao:update_pk_diff(
            Book,
            Book#book{id = 2, title = Title},
            NewTBooks,
            ?S
        ),

    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET id = $1, title = $2 WHERE id = $3">>,
            types = [int, varchar, int],
            values = [2, Title, 1]
        },
        Q1#mekao_query{
            body = iolist_to_binary(QBody1)
        }
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET title = $1 WHERE id = $2">>,
            types = [varchar, int],
            values = [Title, 1]
        },
        Q2#mekao_query{
            body = iolist_to_binary(QBody2)
        }
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"UPDATE books SET id = $1, title = $2 WHERE id = $3">>,
            types = [int, varchar, int],
            values = [2, Title, 1]
        },
        Q3#mekao_query{
            body = iolist_to_binary(QBody3)
        }
    ).


delete_pk_test() ->
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE id IS NULL">>,
            types = [], values = []
        },
        mk_call(delete_pk, book(undefined))
    ),
    ?assertMatch(
        #mekao_query{
            body = <<"DELETE FROM books WHERE id = $1">>,
            types = [int], values = [1]
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

mk_call(CallName, E, Opts, Table, S) ->
    {ok, Q = #mekao_query{body = B}} = mekao:CallName(E, Opts, Table, S),
    Q#mekao_query{body = iolist_to_binary(B)}.

mk_ph(N) when is_integer(N) ->
    [$$ | integer_to_list(N)].

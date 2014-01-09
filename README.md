mekao
=====

The goal of this library is to construct basic SQL queries and to allow
to extend build results in complex cases. Generated queries are not vendor
specific.

Main assumption was that records are used to represent DB data.


status
======

Alpha. It is under active development, any feedback are appreciated!


usage
=====

1. Include mekao.hrl
```erlang
    -include_lib("mekao/include/mekao.hrl").
```

2. Describe your record' fields (note that number and order of columns same
as in record)
```erlang
    -record(book, {id, title, author}).

    -define(TABLE_BOOKS, #mekao_table{
        name    =  <<"books">>,
        columns = [
            #mekao_column{name = <<"id">>, type = int, key = true, ro = true},
            #mekao_column{name = <<"title">>, type = varchar},
            #mekao_column{name = <<"author">>, type = varchar}
        ]
    }).
```

3. Adjust some domain specific settings
```erlang
    -define(MEKAO_SETTINGS, #mekao_settings{
        placeholder = fun (_, Pos, _) -> [$$ | integer_to_list(Pos)] end,
        is_null     = fun(V) -> V == undefined end,
        returning   =
            fun
                (insert, #mekao_table{columns = Columns}) ->
                    ColumnNames = mekao_utils:intersperse(
                        [ColumnName || #mekao_column{name = ColumnName} <- Columns],
                        <<", ">>
                    ),
                    [<<"RETURNING ">>, ColumnNames];
                (_, _) ->
                    <<"">>
            end
    }).
```

4. Use it:
```erlang
    {ok, #mekao_query{body = Q, types = Types, values = Values}}
        = mekao:insert(#book{title = <<"SQL rocks!">>, author = <<"Smarty">>}),

        <<"INSERT INTO books (title, author) VALUES ($1, $2)",
            " RETURNING id, title, author">> = iolist_to_binary(Q),
        [varchar, varchar] = Types,
        [<<"SQL rocks!">>, <<"Smarty">>] = Values.
```

documentation
=============
Tests are the best source of documentation for now.

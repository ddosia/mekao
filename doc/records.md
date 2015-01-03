# ToC
1.   [#mekao_settings{}](#mekao_settings)
2.   [#mekao_table{}](#mekao_table)
3.   [#mekao_column{}](#mekao_column)


## #mekao_settings{}
This record contains general setting.
```erlang
-record(mekao_settings, {
    placeholder :: fun( ( mekao:column()
                        , Num :: non_neg_integer()
                        , Val :: term()
                        ) -> iodata()),

    limit :: undefined
           | fun( ( mekao:'query'(#mekao_select{})
                  , RowCount :: non_neg_integer()
                  , Offset :: non_neg_integer()
                  ) -> mekao:'query'(#mekao_select{})),

    returning :: undefined
               | fun(( insert | update | delete, mekao:table()) -> iodata()),

    is_null = fun mekao_utils:is_null/1 :: fun((Value :: term()) -> boolean())
}).
```

### placeholder
Produce WHERE clause placeholders.

Example:
```erlang
#mekao_settings{
    placeholder = fun (_, Pos, _) -> [$$ | integer_to_list(Pos)] end
}. %% <<"... WHERE field1 = $1, field2 = $2...">>
```
See [#mekao_column{}](#mekao_column)

### limit
If defined will be used in select functions to limit result.

Example:
```erlang
#mekao_settings{
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
}.
```

### returning
Produce RETURNING clause for INSERT, UPDATE or DELETE queries.

Example:
```erlang
#mekao_settings{
    returning = fun (_, _) -> <<"RETURNING id">> end
}. %% <<"INSERT INTO... RETURNING id">>
```

### is_null
Check value and return true if it `IS NULL`, false otherwise. Is used to
compose WHERE clause and produce `WHERE column IS NULL` instead of
`WHERE column = NULL`.

Example:
```erlang
#mekao_settings{
    is_null = 
        fun (undefined) -> true 
            (null) -> true
            (_) -> false
        end
}.
```

## #mekao_table{}
This record describes SQL table.
```erlang
-record(mekao_table, {
    name            :: iodata(),
    columns = []    :: [ mekao:column()
                       % entity record's field on the same pos is out of
                       % interest
                       | '$skip' ],
    %% order by column position or by arbitrary expression
    order_by = []   :: [ non_neg_integer() % record's field pos
                       | iodata()          % arbitrary expression
                       | { non_neg_integer() | iodata()
                         , { asc | desc | default
                           , nulls_first | nulls_last | default}
                         }
                       ]
}).
```

### name
Table name

Example:
```erlang
#mekao_table{
    name = <<"books">>
}.
```

### columns
List of table columns. Order matters. Each column position must be the same
as a corresponding record's field.

Example:
```erlang
-record(book, {id, isbn, not_db_field}).

#mekao_table{
    columns = [
        #mekao_column{name = <<"id">>}, %% first position, same as #book.id
        #mekao_column{name = <<"isbn">>} %% second position, same as #book.isbn
        '$skip'
    ]
}.
```
The last column `#book.not_db_field` illustrating possibility of having some
record's fields, which are not represented by any particular SQL table's
column, but serves just for internal usage.

See [#mekao_column{}](#mekao_column)

### order_by
List of positions and/or arbitrary expressions to sort result of select queries.

Example:
```erlang
-record(book, {id, isbn}).

#mekao_table{
    order_by = [#book.isbn, <<"isbn">>]
}.
%% SELECT id, isbn FROM ... ORDER BY 2, isbn
%% although you could see that `isbn` have `2` position in SELECT statement,
%% #mekao_table.order_by expects number `3` (`#book.isbn = 3`).
```

## #mekao_column{}
This record is intended to describe particular column.
```erlang
-record(mekao_column, {
    name        :: iodata(),        %% sql column name
    type        :: term(),          %% sql datatype, acceptable by underlying
                                    %% driver
    key = false :: boolean(),       %% primary key part
    ro  = false :: boolean(),       %% readonly
    transform   :: undefined
                 | fun ((Val :: term()) -> NewVal :: term())
}).
```

### name
Column name as in SQL table.

Example:
```erlang
#mekao_column{
    name = <<"author">>
}.
```

### type
Column type. Could be used later by underlying db driver.

Example:
```erlang
#mekao_column{
    type = datetime
}.
```

### key
True if this is a part of primary key, false otherwise.

Have special meaning for some operations suffixed whit `_pk`
(i.e. `mekao:select_pk/3`).

### ro
True if this field is read only.

Read only fields will be skipped during inserts and updates.

### transform
Sometimes values should be grained before we feed it to db driver.

Example:
```erlang
#mekao_column{
    name = <<"mamal_type">>
    type = int,
    transform =
        fun (human) -> 0;
            (dog) -> 1;
            (cat) -> 2;
            (whale) -> 65535
        end
}.
```

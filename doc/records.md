# ToC
1.   [#mekao_settings{}](#mekao_settings)
2.   [#mekao_table{}](#mekao_table)
3.   [#mekao_column{}](#mekao_column)


## #mekao_settings{}
This record contains general setting.

### placeholder
```erlang
#mekao_settings{
    placeholder :: fun((mekao:column(), Num :: non_neg_integer(), Val :: term()) -> iodata())
}.
```
Purpose of this function is to produce WHERE clause placeholders.

Example:
```erlang
#mekao_settings{
    placeholder = fun (_, Pos, _) -> [$$ | integer_to_list(Pos)] end
}. %% <<"... WHERE field1 = $1, field2 = $2...">>
```
See [#mekao_column{}](#mekao_column)

### returning
```erlang
#mekao_settings{
    returning :: undefined | fun((insert | update | delete, mekao:table()) -> iodata())
}.
```
This fun is intended to produce RETURNING clause for INSERT, UPDATE or DELETE
queries.

Example:
```erlang
#mekao_settings{
    returning = fun (_, _) -> <<"RETURNING id">> end
}. %% <<"INSERT INTO... RETURNING id">>
```

### is_null
```erlang
#mekao_settings{
    is_null :: fun((Value :: term()) -> boolean())
}.
```
This fun is intended to check value and return true if it IS NULL,
false otherwise. Used when compose WHERE clause and produce
`WHERE column IS NULL` instead of `WHERE column = NULL`

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

### name
```erlang
#mekao_settings{
    name :: iodata()
}.
```
Table name

### columns
```erlang
#mekao_settings{
    columns = [] :: [mekao:column()]
}.
```
List of table columns. Order matters. Each column position must be the same
as a corresponding record field.

Example:
```erlang
-record(book, {id, isbn}).

#mekao_table{
    columns = [
        #mekao_column{name = <<"id">>}, %% first position, same as #book.id
        #mekao_column{name = <<"isbn">>} %% second position, same as #book.isbn
    ]
}.
```
See [#mekao_column{}](#mekao_column)

### order_by
```erlang
#mekao_settings{
    order_by = [] :: [ non_neg_integer() % record's field pos
                     | iodata()          % arbitrary expression
                     | { non_neg_integer() | iodata()
                       , { asc | desc | default
                         , nulls_first | nulls_last | default}
                       }
                     ]
}.
```
List of record's field positions and/or expressions to sort result of
`select*` queries.

Important note, this is not a position of column in SELECT.

Example:
```erlang
-record(book, {id, isbn}).

#mekao_table{
    order_by = [#book.isbn]
}.
%% SELECT id, isbn FROM ... ORDER BY 2
%% although you could see that `isbn` have `2` position in SELECT statement,
%% #mekao_table.order_by expects number `3` (`#book.isbn = 3`).
```

## #mekao_column{}
This record is intended to describe particular column.

### name
```erlang
#mekao_settings{
    name :: iodata()
}.
```
Column name as it is named in SQL table.

### type
```erlang
#mekao_settings{
    type :: term()
}.
```
Column type. Could be used later by underlying db driver.

Example:
```erlang
#mekao_column{
    type = datetime
}.
```

### key
```erlang
#mekao_settings{
    key = false :: boolean()
}.
```
True if this is a primary key, false otherwise.

Have special meaning for some operations suffixed whit `_pk`
(i.e. `mekao:select_pk/3`).

### ro
```erlang
#mekao_settings{
    ro  = false :: boolean()
}.
```
True if this field is read only.

Read only fields will be skipped during inserts and updates.

### transform
```erlang
#mekao_settings{
    transform :: undefined | fun ((Val :: term()) -> NewVal :: term())
}.
```
Sometimes values should be grained before we feed it to db driver. Example:
```erlang
#mekao_column{
    name = <<"mamal_type">>
    type = int,
    transform =
        fun (human) -> 0;
            (dog) -> 1;
            (cat) -> 2;
            (whale) -> 65531
        end
}.
```

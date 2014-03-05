# ToC
1.   [#mekao_settings{}](#mekao_settings)
2.   [#mekao_table{}](#mekao_table)
3.   [#mekao_column{}](#mekao_column)
3.   [#mekao_query{}](#mekao_query)


## #mekao_settings{}
This record contains general setting.

### placeholder
This fun accepts 3 args:
* [#mekao_column{}](#mekao_column)
* `non_neg_integer()`, column position (see [#mekao_query.next_ph_num](#next_ph_num))
* `term()`, field value

and should return placeholder, which will be used in query.

This example illustrate how to get numerical placeholders:
```erlang
#mekao_settings{
    placeholder = fun (_, Pos, _) -> [$$ | integer_to_list(Pos)] end
}. %% <<"... WHERE field1 = $1, field2 = $2...">>
```

### returning
This fun is intended to form RETURNING clause for insert, update or delete
queries. Accepts 2 arguments:
* `insert | update | delete`, type of the query
* [#mekao_table{}](#mekao_table), corresponding table

Example:
```erlang
#mekao_settings{
    returning = fun (_, _) -> <<"RETURNING id">> end
}. %% <<"INSERT INTO... RETURNING id">>
```

### is_null
This fun is intended to check value and return true if it IS NULL,
false otherwise.

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
This record is intended to describe SQL table.

### name
Table name

### columns
List of table columns. Order matters. Each column position must be the same
as a corresponding record field.

Example:
```erlang
-record(book, {id, isbn}).

#mekao_table{
    columns = [
        #mekao_column{name = <<"id">>}, %% first in the list, same as in #book{}
        #mekao_column{name = <<"isbn">>} %% second in the list
    ]
}.
```
See [#mekao_column{}](#mekao_column)

### order_by

List of record's field positions and/or expressions to sort result of
`select*` queries.

Important note, this is not a position of column, but record's field number.

Example:
```erlang
-record(book, {id, isbn}).

#mekao_table{
    order_by = [#book.isbn]
}.
%% SELECT id, isbn FROM ... ORDER BY 2
%% although #book.isbn = 3
```

## #mekao_column{}
This record is intended to describe particular column.

### name
Field name as it is named in SQL table.

### type
Column type. Used later in [#mekao_query.types](types) by underlying db driver
(or not, depends on driver).

Example:
```erlang
#mekao_column{
    type = datetime
}.
```

### key
True if this is primary key, false otherwise. Have special meaning for some
operations suffixed whit `_pk` (i.e. `mekao:select_pk/3`).

### ro
True if this field is read only. Skipped during inserts and updates.

### transform
Sometimes values should be grained before we feed it to db driver:

Example:
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


## #mekao_query{}
This record contains prepared or built query, types and values.

### body
Query body. If query has been built then it is an `iolist` or one of the
`#mekao_select{} | #mekao_insert{} | #mekao_update{} | #mekao_delete{}`.

### types
All the types [#mekao_column.type](#type) which has been used during query
construction. Order of occurrence is preserved and corresponds
to [#mekao_query.values](#values).

### values
All values which has been used during query construction.  Order of occurrence
is preserved and corresponds to [#mekao_query.types](#types)

### next_ph_num
This field contains next number, which could be used to make a [#mekao_settings.placeholder](#placeholder) if you manually change a query and add some params.

# mekao
This library will help you to construct SQL queries. It have no other
facilities: no pools, no caching, no RDBMS specific code.
Main assumption is that records are used to represent DB data.

[![Build Status](https://secure.travis-ci.org/ddosia/mekao.png?branch=master)](http://travis-ci.org/ddosia/mekao)


# ToC
1.  [Thesis](#thesis)
2.  [Basic usage](#usage)
3.  [Install](#install)
4.  [Records](#records)
5.  [Selectors](#selectors)
6.  [Specs](doc/specs.md)


# Thesis
SQL is complex language. There are variety of weird cases when you will be
not satisfied with query, generated by any tool. Sophisticated tools aimed
to hide this complexity and eventually became as complex as SQL itself and even
more: they substitute well defined SQL by vague DSL, add caching layers, do
fancy type conversions.

Goals of this library:
* to cover most basic cases;
* to give you an ability to adjust generated query;
* to be embeddable into other libraries;
* to be vendor agnostic(generated SQL is good for any RDBMS, but could be tuned to
match yours).


# Usage
Suppose that we have table `books` in our SQL db:

| Column    | Type      | Attributes                  |
|-----------|-----------|-----------------------------|
| id        | int       | Primary key, read only      |
| isbn      | varchar   |                             |
| title     | varchar   |                             |
| author    | varchar   |                             |
| created   | timestamp | Read-only                   |

To begin to use *mekao* you'll need couple of things:
* make a record with same fields as in SQL table you are interested in;
* describe table in terms of *mekao*;
* write some general settings for all queries (like how exactly LIMIT queries
are constructed for your RDBMS dialect, or how placeholders are looks like for
you DB driver).

### make a record
```erlang
-record(book, {id, isbn, title, author, created}).
```


### describe a table
```erlang
-include_lib("mekao/include/mekao.hrl").

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
```
Pay attention: each field in `#mekao_table{}` must be at the same position as
corresponding field in `#book{}` (like `title` column have 3rd position in both
records).


### write general settings
```erlang
-define(S, #mekao_settings{
    %% our placeholders will look like:
    %% ... WHERE id = $1 AND isbn = $2
    placeholder = fun (_, Pos, _) -> [$$ | integer_to_list(Pos)] end
}).
```


### glimpse of usage

```erlang
fetch_book(SelectBook) ->
    {ok, #mekao_query{
        body = Q, types = Types, values = Vals
    }} = mekao:select(SelectBook, ?TABLE_BOOKS, ?S),
    {iolist_to_binary(Q), Types, Vals}.

update_book(SetBook, WhereBook) ->
    {ok, #mekao_query{
        body = Q, types = Types, values = Vals
    }} = mekao:update(SetBook, WhereBook, ?TABLE_BOOKS, ?S),
    {iolist_to_binary(Q), Types, Vals}.

%%... snip ...

{<<"SELECT id, isbn, title, author, created FROM books WHERE id = $1">>,
    [int], [1]
} = fetch_book(#book{id = 1, _ = '$skip'}),

{<<"SELECT id, isbn, title, author, created FROM books"
    " WHERE author LIKE $1">>, [varchar], [<<"%Joe%">>]
} = fetch_book(
    #book{author = {'$predicate', like, <<"%Joe%">>}, _ = '$skip'}
),

{<<"UPDATE books SET author = $1 WHERE id IS NULL">>,
    [varchar], [<<"Joe">>]
} = update_book(
    #book{author = <<"Joe">>, _ = '$skip'}, %% SET clause
    #book{id = undefined, _ = '$skip'}  %% WHERE clause
),

%%... snip ...
```

You definitely noticed `'$skip'` atom. When you construct record
like this, every other field will have `'$skip'` as a value:
```erlang
1> #book{id = 1, _ = '$skip'}.
#book{id = 1, isbn = '$skip', title = '$skip',
      author = '$skip', created = '$skip'}
```
This instructs *mekao* that you don't want to include other fields in query.

You may wonder about `iolist_to_binary/1` trick. All queries generated by
*mekao* have a type `iodata()`. This means there could be mixed strings,
binaries, chars, nested lists of strings and so on. Some drivers do accept
`iodata()`, others do not. This made in the sake of performance, it is up to
application to convert this to any acceptable form.

Placeholders `$1` and `$2` were generated with help of user-defined
`#mekao_settings.placeholder` function.

If you want to extend resulted query use `mekao:prepare_*` set of queries
instead.

For more examples please see [test/mekao_tests.erl](test/mekao_tests.erl).

# Install
Add this to `rebar.config`
```erlang
{deps, [
    {mekao, {git, "git://github.com/ddosia/mekao.git", {branch, "v0"}}}
]}.

```

Alternatively use [hex](https://hex.pm/packages/mekao).

Project follows [SemVer](http://semver.org) versioning conventions. Backward
incompatible changes will result in a new branch, named after *MAJOR* version,
i.e. *v0*, *v1*, *v2* and so on. Make sure that your project depends
on particular branch and not on master.


# Records
* [#mekao_settings{}](doc/records.md#mekao_settings)
* [#mekao_table{}](doc/records.md#mekao_table)
* [#mekao_column{}](doc/records.md#mekao_column)


# Selectors
Selectors is a way to adjust `WHERE` clause. When you pass record to *mekao*
each field may contain regular value, or special predicate term.

| SQL       | predicate                                     |
| --------- | --------------------------------------------- |
| `=`       | `{'$predicate',  '=', term()}`                |
| `<>`      | `{'$predicate', '<>', term()}`                |
| `>`       | `{'$predicate',  '>', term()}`                |
| `>=`      | `{'$predicate', '>=', term()}`                |
| `<`       | `{'$predicate',  '<', term()}`                |
| `<=`      | `{'$predicate', '<=', term()}`                |
| `LIKE`    | `{'$predicate', like, term()}`                |
| `BETWEEN` | `{'$predicate', 'between', term(), term()}`   |
| `IN`      | `{'$predicate', in, [term(), ...]}`           |
| `NOT`     | `{'$predicate', not, predicate()}`            |

Example:
```erlang
DT1 = {{2013, 1, 1}, {0, 0, 0}},
DT2 = {{2014, 1, 1}, {0, 0, 0}},

mekao:select(
    #book{
        created = {'$predicate', between, DT1, DT2},
        _       = '$skip'
    }, ?TABLE_BOOKS, ?S
).
%% SELECT id, isbn, title, author, created FROM books
%% WHERE created BETWEEN $1 AND $2
```

see `mekao:selector()` type spec.

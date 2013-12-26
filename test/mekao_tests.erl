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
    returning   = fun (_, _) -> [] end,
    is_null     = fun(V) -> V == undefined end
}).

book(Id) ->
    #book{
        id      = Id,
        isbn    = <<"978-1593274351">>,
        title   = <<"Learn You Some Erlang for Great Good!: A Beginner's Guide">>,
        author  = <<"Fred Hebert">>,
        created = calendar:now_to_datetime(os:timestamp())
    }.

select_pk(B) ->
    {Q, Types, Vals} = mekao:select_pk(B, ?TABLE_BOOKS, ?S),
    {iolist_to_binary(mekao:build(Q)), Types, Vals}.

select(B) ->
    {Q, Types, Vals} = mekao:select(B, ?TABLE_BOOKS, ?S),
    {iolist_to_binary(mekao:build(Q)), Types, Vals}.


is_null_test() ->
    ?assert(
        {<<"SELECT id, isbn, title, author, created FROM books",
           " WHERE author IS NULL;">>, [varchar], [undefined]} ==
        select(#book{author = undefined, _ = '$skip'})
    ).


select_pk_test() ->
    ?assert(
        {<<"SELECT id, isbn, title, author, created FROM books",
            " WHERE id = $1;">>, [int], [1]} == select_pk(book(1))
    ).

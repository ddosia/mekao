-record(mekao_query, {
    body,
    types  = []     :: [term()],
    values = []     :: [term()],
    next_ph_num = 1 :: non_neg_integer()
}).

-record(mekao_select, {
    columns     :: mekao:iotriple(),
    table       :: mekao:iotriple(),
    where       :: mekao:iotriple(),
    order_by    :: mekao:iotriple()
}).

-record(mekao_insert, {
    table       :: mekao:iotriple(),
    columns     :: mekao:iotriple(),
    values      :: mekao:iotriple(),
    returning   :: mekao:iotriple()
}).

-record(mekao_update, {
    table       :: mekao:iotriple(),
    set         :: mekao:iotriple(),
    where       :: mekao:iotriple(),
    returning   :: mekao:iotriple()
}).

-record(mekao_delete, {
    table       :: mekao:iotriple(),
    where       :: mekao:iotriple(),
    returning   :: mekao:iotriple()
}).

-record(mekao_column, {
    name        :: iodata(),        %% sql column name
    type        :: term(),          %% sql datatype, acceptable by underlying
                                    %% driver
    key = false :: boolean(),       %% primary key part
    ro  = false :: boolean(),       %% readonly
    transform   :: undefined
                 | fun ((Val :: term()) -> NewVal :: term())
}).

-record(mekao_table, {
    name            :: iodata(),
    columns = []    :: [mekao:column()],
    %% order by column position or by arbitrary expression
    order_by = []   :: [ non_neg_integer() % record's field pos
                       | iodata()          % arbitrary expression
                       | { non_neg_integer() | iodata()
                         , { asc | desc | default
                           , nulls_first | nulls_last | default}
                         }
                       ]
}).

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

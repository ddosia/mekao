-record(mekao_column, {
    name        :: binary() | iolist(), %% sql column name
    type        :: term(),          %% sql datatype, acceptable by underlying
                                    %% driver
    key = false :: boolean(),       %% primary key part
    ro  = false :: boolean(),       %% readonly
    transform   :: undefined
                 | fun ((Val :: term()) -> NewVal :: term())
}).

-record(mekao_table, {
    name         :: binary() | iolist(),
    columns = [] :: [mekao:column()]
}).

-record(mekao_settings, {
    %% different db drivers have different placeholders.
    %% pgsql accepts placeholders in form of `"$1, $2, ... $N"'.
    %% odbc driver accepts phs in form of `"?, ?, ..., ?"'
    placeholder                 :: fun( ( mekao:column()
                                        , Num :: non_neg_integer()
                                        , Val :: term()
                                        ) -> binary() | iolist()),

    %% forms "returning" string for insert/update queries
    returning                   :: undefined
                                 | fun( ( insert | update | delete
                                        , mekao:table()
                                        ) -> binary() | iolist()),

    %% it is up to application what erlang term represents NULL
    %% (for instance 'undefined' or 'null')
    is_null                     :: fun((Value :: term()) -> boolean())
}).

-record(mekao_query, {
    body,
    types  = []     :: [term()],
    values = []     :: [term()],
    next_ph_num = 1 :: non_neg_integer()
}).

-record(mekao_select, {
    columns     :: binary() | iolist(),
    table       :: binary() | iolist(),
    where       :: binary() | iolist()
}).

-record(mekao_insert, {
    table       :: binary() | iolist(),
    columns     :: binary() | iolist(),
    values      :: binary() | iolist(),
    returning   :: binary() | iolist()
}).

-record(mekao_update, {
    table       :: binary() | iolist(),
    set         :: binary() | iolist(),
    where       :: binary() | iolist(),
    returning   :: binary() | iolist()
}).

-record(mekao_delete, {
    table       :: binary() | iolist(),
    where       :: binary() | iolist(),
    returning   :: binary() | iolist()
}).

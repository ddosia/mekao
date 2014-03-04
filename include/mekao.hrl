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
    name         :: iodata(),
    columns = [] :: [mekao:column()]
}).

-record(mekao_settings, {
    %% different db drivers have different placeholders.
    %% pgsql accepts placeholders in form of `"$1, $2, ... $N"'.
    %% odbc driver accepts phs in form of `"?, ?, ..., ?"'
    placeholder                 :: fun( ( mekao:column()
                                        , Num :: non_neg_integer()
                                        , Val :: term()
                                        ) -> iodata()),

    %% forms "returning" string for insert/update queries
    returning                   :: undefined
                                 | fun( ( insert | update | delete
                                        , mekao:table()
                                        ) -> iodata()),

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
    columns     :: iodata(),
    table       :: iodata(),
    where       :: iodata()
}).

-record(mekao_insert, {
    table       :: iodata(),
    columns     :: iodata(),
    values      :: iodata(),
    returning   :: iodata()
}).

-record(mekao_update, {
    table       :: iodata(),
    set         :: iodata(),
    where       :: iodata(),
    returning   :: iodata()
}).

-record(mekao_delete, {
    table       :: iodata(),
    where       :: iodata(),
    returning   :: iodata()
}).

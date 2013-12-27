-record(mekao_column, {
    name        :: iolist(),        %% sql column name
    type        :: term(),          %% sql datatype, acceptable by underlying
                                    %% driver
    key = false :: boolean(),       %% primary key part
    ro  = false :: boolean()        %% readonly
}).

-record(mekao_table, {
    name         :: iolist(),
    columns = [] :: [mekao:column()]
}).

-record(mekao_settings, {
    %% different db drivers have different placeholders.
    %% pgsql accepts placeholders in form of `"$1, $2, ... $N"'.
    %% odbc driver accepts phs in form of `"?, ?, ..., ?"'
    placeholder                 :: fun( ( Pos :: non_neg_integer()
                                        , Val :: term()
                                        ) -> iolist()),

    %% forms "returning" string for insert/update queries
    returning                   :: undefined
                                 | fun( ( insert | update | delete
                                        , mekao:table()
                                        ) -> iolist()),

    %% it is up to application what erlang term represents NULL
    %% (for instance 'undefined' or 'null')
    is_null                     :: fun((Value :: term()) -> boolean())
}).

-record(mekao_select, {
    columns     :: iolist(),
    table       :: iolist(),
    where       :: iolist()
}).

-record(mekao_insert, {
    table       :: iolist(),
    columns     :: iolist(),
    values      :: iolist(),
    returning   :: iolist()
}).

-record(mekao_update, {
    table       :: iolist(),
    set         :: iolist(),
    where       :: iolist(),
    returning   :: iolist()
}).

-record(mekao_delete, {
    table       :: iolist(),
    where       :: iolist(),
    returning   :: iolist()
}).

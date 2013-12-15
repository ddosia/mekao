-record(mekao_field, {
    name        :: iolist(),        %% sql field name
    type        :: mekao:type(),    %% sql datatype, acceptable by underlying
                                    %% driver
    key = false :: boolean(),       %% primary key part
    ro  = false :: boolean()        %% readonly
}).

-record(mekao_table, {
    name        :: iolist(),
    fields = [] :: [mekao:field()]
}).

-record(mekao_settings, {
    %% different db drivers have different placeholders.
    %% pgsql accepts placeholders in form of `"$1, $2, ... $N"'.
    %% odbc driver accepts phs in form of `"?, ?, ..., ?"'
    ph_fun                      :: fun( ( Pos :: non_neg_integer()
                                        , Val :: term()
                                        ) -> iolist()),
    
    %% forms "return" string for insert/update queries
    ret_fun                     :: fun((mekao:table()) -> iolist())
}).

-record(field, {
    name        :: iolist(),    %% sql field name
    type,                       %% sql datatype, acceptable by underlying driver
    key = false :: boolean(),   %% primary key part
    ro  = false :: boolean()    %% readonly
}).

-record(table, {
    name        :: iolist(),
    fields = []
}).

-record(s, {
    tables = orddict:new()      :: orddict:orddict(),

    %% different db drivers have different placeholders.
    %% pgsql accepts placeholders in form `"$1, $2, ... $N"'.
    %% odbc driver accepts phs in form `"?, ?, ..., ?"'
    ph_fun                      :: fun((non_neg_integer()) -> iolist()),
    
    %% forms "return" string for insert/update queries
    ret_fun                     :: fun((table()) -> iolist())
}).

-module(mekao_utils).

%% API
-export([
    identity/1,
    intersperse/2, intersperse/3,
    intersperse4/3
]).


%% ===================================================================
%% API functions
%% ===================================================================

-spec identity(term()) -> term().
identity(X) -> X.

intersperse(List, Sep) ->
    intersperse(List, Sep, fun identity/1).

-spec intersperse( List :: list()
                 , Separator :: term()
                 , ValFun :: fun((term()) -> term())
                 ) -> list().
intersperse([], _, _) ->
    [];
intersperse([Item], _, ValFun) ->
    [ValFun(Item)];
intersperse([Item | Items], Sep, ValFun) ->
    [ValFun(Item), Sep | intersperse(Items, Sep, ValFun)].


intersperse4({[], [], [], []}, _, _) ->
    [];
intersperse4({[I1], [I2], [I3], [I4]}, _, ValFun) ->
    [ValFun({I1, I2, I3, I4})];
intersperse4({[I1 | I1s], [I2 | I2s], [I3 | I3s], [I4 | I4s]}, Sep, ValFun) ->
    [ValFun({I1, I2, I3, I4}), Sep | intersperse4({I1s, I2s, I3s, I4s}, Sep, ValFun)].


%%%===================================================================
%%% Internal functions
%%%===================================================================


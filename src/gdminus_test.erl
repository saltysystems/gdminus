-module(gdminus_test).

-export([
    regenerate/1,
    regenerate/0,
    test_insert/0
]).

regenerate(verbose) ->
    leex:file("src/gdminus_scan.xrl"),
    yecc:file("src/gdminus_parse.yrl", {verbose, true}).

regenerate() ->
    leex:file("src/gdminus_scan.xrl"),
    yecc:file("src/gdminus_parse.yrl").

test_insert() ->
    gdminus_int:init(),
    F1 = fun([X]) -> math:erf(X) end,
    gdminus_int:insert_function("erf", F1),
    {Out, _Err, _St} = gdminus_int:do("print(erf(0.42))"),
    io:format("Output is: ~p~n", [Out]),
    gdminus_int:destroy().

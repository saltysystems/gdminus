-module(gdminus_test).

-export([
    regenerate/1,
    regenerate/0,
    lex_file/1,
    parse_file/1,
    interpret_file/1
]).

lex_file(File) ->
    {ok, F} = file:read_file(File),
    Fn = binary:bin_to_list(F),
    {ok, L, _Lines} = gdminus_scan:string(Fn),
    gdminus_scan:normalize(L).

parse_file(File) ->
    {ok, AST} = gdminus_parse:parse(lex_file(File)),
    AST.

interpret_file(File) ->
    AST = parse_file(File),
    gdminus_interpreter:walk(AST).

regenerate(verbose) ->
    leex:file("src/gdminus_scan.xrl"),
    yecc:file("src/gdminus_parse.yrl", {verbose, true}).
regenerate() ->
    leex:file("src/gdminus_scan.xrl"),
    yecc:file("src/gdminus_parse.yrl").

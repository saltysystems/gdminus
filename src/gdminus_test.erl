-module(gdminus_test).

-export([
    regenerate/0,
	lex_file/1
	%parse_file/1
	]).

lex_file(File) ->
	{ok, F} = file:read_file(File),
	Fn = binary:bin_to_list(F),
	{ok, L, _Lines} = gdminus_scan:string(Fn),
	gdminus_scan:normalize(L).

regenerate() ->
    leex:file("src/gdminus_scan.xrl"),
    yecc:file("src/gdminus_parse.yrl").
    

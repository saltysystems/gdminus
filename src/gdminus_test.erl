-module(gdminus_test).

-export([
	lex_file/1
	%parse_file/1
	]).

lex_file(File) ->
	{ok, F} = file:read_file(File),
	Fn = binary:bin_to_list(F),
	{ok, L, _Lines} = gdminus_scan:string(Fn),
	L.
	%gdminus_scan:normalize(L).

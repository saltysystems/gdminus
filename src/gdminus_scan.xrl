Definitions.

NUMBER = [0-9]+
FLOAT = ([0-9]*[.])?[0-9]+
WS = [\s]+
INDENT = \n[\s]*|\n[\t]*
LB = \n|\r\n|\r
NAME = [A-Za-z_][A-Za-z0-9_]*
COMP = <|>|==|<=|>=|!=
COMMENT = #.*

Rules.
\"(\\.|\\\n|[^"\\])*\" : string_token(TokenChars, TokenLen, TokenLine).
\'(\\.|\\\n|[^'\\])*\' : string_token(TokenChars, TokenLen, TokenLine).
{NAME}      : name_token(TokenChars, TokenLine).
{NUMBER}    : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOAT}     : {token, {number, TokenLine, handle_float(TokenChars)}}.
!=          : {token, {'!=', TokenLine, TokenChars}}.
==          : {token, {'==', TokenLine, TokenChars}}.
<=          : {token, {'<=', TokenLine, TokenChars}}.
>=          : {token, {'>=', TokenLine, TokenChars}}.
<           : {token, {'<', TokenLine, TokenChars}}.
>           : {token, {'>', TokenLine, TokenChars}}.
\=          : {token, {'=', TokenLine}}.
\+          : {token, {'+', TokenLine}}.
\%          : {token, {'%', TokenLine}}.
\-          : {token, {'-', TokenLine}}.
\*          : {token, {'*', TokenLine}}.
\/          : {token, {'/', TokenLine}}.
\(          : {token, {'(', TokenLine}}.
\)          : {token, {')', TokenLine}}.
\#          : {token, {'#', TokenLine}}.
\:          : {token, {':', TokenLine}}.
\"          : {token, {'"', TokenLine}}.
\'          : {token, {quote, TokenLine}}.
\.          : {token, {'.', TokenLine}}.
\,          : {token, {',', TokenLine}}.
\[          : {token, {'[', TokenLine}}.
\]          : {token, {']', TokenLine}}.
\{			: {token, {'{', TokenLine}}.
\}			: {token, {'}', TokenLine}}.
{INDENT}    : evaluate_indent_level(TokenChars, TokenLine).
{WS}        : skip_token.
{COMMENT}   : skip_token. % comments
{LB}       : skip_token.

Erlang code.
-export([is_keyword/1]).
-export([normalize/1]).

% The gdminus scanner will produce multiple levels of indentation per token,
% this function will flatten the results such that multiple indent or dedent
% tokens are printed per script line
normalize(List) ->
	% The resulting list will be built backwards and deep
	lists:reverse(lists:flatten(normalize(List, []))).

normalize([], Acc) ->
	erlang:erase(mode),
	erlang:erase(s_tabstop),
	erlang:erase(s_indent),
	Acc;
normalize([{Type, Line, Number} | T ], Acc) when Type == indent; Type == dedent ->
	%io:format("Found an indent or dedent token~n"),
	Result = explode({Type, Line, Number}),
	%io:format("Exploded tokens, result: ~p~n", [Result]),
	% Attach the resulting head to the accumulator 
	normalize(T, [ Result | Acc ]);
normalize([H|T], Acc) ->
	% Do nothing with lines that don't match indent or dedent
	%io:format("Doing nothing with line ~p~n", [H]),
	normalize(T, [ H |Acc]).


explode({Type, Line, Number}) ->
	explode({Type, Line, Number}, []).

explode({_Type, _Line, Number}, Acc) when Number == 0 ->
	Acc;
explode({Type, Line, Number}, Acc) ->
	NewAcc = [ {Type, Line} | Acc ],
	explode({Type, Line, Number - 1}, NewAcc).
	

tab_or_space([Head|Rest]) when Head == 10 ->
	tab_or_space(Rest);
tab_or_space(["\t"|_Rest]) ->
	tab;
tab_or_space("\t") ->
	tab;
tab_or_space(_) ->
	space.
	

evaluate_indent_level(Chars, Line) ->
	% Yet more abuse of the process dictionary
	% We should set the mode the first time we encounter a indent token
	case get(mode) of
		undefined ->
			% Remove the initial newline
			case tab_or_space(Chars) of
				tab ->
					put(mode,tab),
					evaluate_indent_level_tab(Chars, Line);
				space ->
					put(mode,space),
					evaluate_indent_level_space(Chars, Line)
			end;
		space ->
			evaluate_indent_level_space(Chars,Line);
		tab ->
			evaluate_indent_level_tab(Chars, Line)
	end.

evaluate_indent_level_space(Chars, Line) ->
	% Represents the indentation level. The regex match looks for a newline
	% followed by any number of \s chars
	Level = length(Chars) - 1,
    % the first time we encounter indentation greater than 0, we record its
    % length to set the tab stop
    TabStop = 
        case get(tabstop_s) of
            undefined -> 
                case Level > 0 of
                    true ->
                        put(tabstop_s, Level),
                        Level;
                    false -> 1 %?
                end;
            T ->
                T
        end,

    CurLevel = Level div TabStop,

	% Compare the existing level to this one
	PrevLevel = 
		case get(indent_s) of
			undefined ->
				0;
			L -> L
		end,
	% Update with the current level
    put(indent_s, CurLevel),

	scope_token(PrevLevel, CurLevel, Line).

evaluate_indent_level_tab(Chars, Line) ->
	% Represents the indentation level. The regex match looks for a newline
	% followed by any number of \t chars
	CurLevel = length(Chars) - 1,

	% Compare the existing level to this one
	PrevLevel = 
		case get(tablev) of
			undefined ->
				0;
			L -> L
		end,
	% Update with the current level
    put(tablev, CurLevel),

	scope_token(PrevLevel, CurLevel, Line).

% If the previous level > current level, emit one or more dedent tokens
scope_token(PrevLevel, CurLevel, Line) when PrevLevel > CurLevel ->
	HowMany = PrevLevel - CurLevel,
	% Put the dedents on the previous line
	{token, {dedent, Line, HowMany}}; 
% If the current level > the previous level, emit an indent token
scope_token(PrevLevel, CurLevel, Line) when CurLevel > 0 ->
	HowMany = CurLevel - PrevLevel,
	{token, {indent, Line + 1, HowMany}};
scope_token(_PrevLevel, CurLevel, _Line) when CurLevel =:= 0 ->
	skip_token.

name_token(Cs, L) ->
    case catch {ok,list_to_binary(Cs)} of
    {ok,Name} ->
        case is_keyword(Name) of
        true -> {token,{name_string(Name),L}};
        false -> {token,{name,L,binary:bin_to_list(Name)}}
        end;
    _ -> {error,"illegal name"}
    end.

name_string(Name) ->
    binary_to_atom(Name, latin1).

%type_token(Cs, L) ->
%    case catch {ok,list_to_binary(Cs)} of
%    {ok,Type} ->
%        case is_type(Type) of
%        true -> {token,{type_string(Type),L}};
%        false -> {token,{type,L,binary:bin_to_list(Type)}}
%        end;
%    _ -> {error,"illegal type"}
%    end.
%
%type_string(Type) ->
%    binary_to_atom(Type, latin1).

string_token(Cs0, Len, L) ->
    Cs1 = string:substr(Cs0, 2, Len - 2),   %Strip quotes
	{token, {string, L, Cs1}}.

handle_float(TokenChars) -> 
    [H|_T] = TokenChars,
    case H of 
        % ASCII '.' symbol
        46 -> 
            list_to_float("0" ++ TokenChars);
        _ ->
            list_to_float(TokenChars)
    end.

%% Logical
is_keyword(<<"and">>) -> true;
is_keyword(<<"or">>) -> true;
is_keyword(<<"not">>) -> true;
%% Control flow
is_keyword(<<"if">>) -> true;
is_keyword(<<"elif">>) -> true;
is_keyword(<<"else">>) -> true;
is_keyword(<<"for">>) -> true;
is_keyword(<<"while">>) -> true;
is_keyword(<<"break">>) -> true;
is_keyword(<<"continue">>) -> true;
is_keyword(<<"pass">>) -> true;
is_keyword(<<"return">>) -> true;
is_keyword(<<"match">>) -> true;
%% Other keywords
is_keyword(<<"as">>) -> true;
is_keyword(<<"assert">>) -> true;
is_keyword(<<"wait">>) -> true;
is_keyword(<<"await">>) -> true;
is_keyword(<<"breakpoint">>) -> true;
is_keyword(<<"class">>) -> true;
is_keyword(<<"class_name">>) -> true;
is_keyword(<<"const">>) -> true;
is_keyword(<<"enum">>) -> true;
is_keyword(<<"extends">>) -> true;
is_keyword(<<"func">>) -> true;
is_keyword(<<"in">>) -> true;
is_keyword(<<"is">>) -> true;
is_keyword(<<"namespace">>) -> true;
is_keyword(<<"preload">>) -> true;
is_keyword(<<"self">>) -> true;
is_keyword(<<"signal">>) -> true;
is_keyword(<<"static">>) -> true;
is_keyword(<<"super">>) -> true;
is_keyword(<<"trait">>) -> true;
is_keyword(<<"var">>) -> true;
is_keyword(<<"void">>) -> true;
is_keyword(<<"yield">>) -> true;
%% Constants
is_keyword(<<"NaN">>) -> true;

is_keyword(_) -> false.

% Builtin types
%is_type(<<"null">>) -> true;
%is_type(<<"int">>) -> true;
%is_type(<<"float">>) -> true;
%is_type(<<"bool">>) -> true;
%is_type(<<"String">>) -> true;
%is_type(<<"Vector2">>) -> true;
%is_type(<<"Rect2">>) -> true;
%is_type(<<"Vector3">>) -> true;
%is_type(<<"Transform2D">>) -> true;
%is_type(<<"Plane">>) -> true;
%is_type(<<"Quat">>) -> true;
%is_type(<<"AABB">>) -> true;
%is_type(<<"Basis">>) -> true;
%is_type(<<"Transform">>) -> true;
%is_type(<<"Color">>) -> true;
%is_type(<<"NodePath">>) -> true;
%is_type(<<"RID">>) -> true;
%is_type(<<"Object">>) -> true;
%is_type(<<"Array">>) -> true;
%is_type(<<"Dictionary">>) -> true;
% 
%is_type(_) -> false.

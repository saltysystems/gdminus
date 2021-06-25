Definitions.

NUMBER = [0-9]+
WS = [\s]+
INDENT = \n[\t]*
%INDENT = \A\t* % doesnt work
LB = \n|\r\n|\r
NAME = [A-Za-z_][A-Za-z0-9_]*
COMP = <|>|==|<=|>=|!=
COMMENT = #.*

Rules.
\"(\\.|\\\n|[^"\\])*\" : string_token(TokenChars, TokenLen, TokenLine).
\'(\\.|\\\n|[^'\\])*\' : string_token(TokenChars, TokenLen, TokenLine).
{NAME}      : name_token(TokenChars, TokenLine).
{NUMBER}    : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{COMP}      : {token, {comparison, TokenLine, TokenChars}}.
\=          : {token, {'=', TokenLine}}.
\+          : {token, {'+', TokenLine}}.
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
%{LB}       : skip_token.

Erlang code.
-export([is_keyword/1]).

evaluate_indent_level(Chars, Line) ->
	% Represents the indentation level. The regex match looks for a newline
	% followed by any number of \t chars
	CurLevel = length(Chars) - 1,

	% Abuse the process dictionary to get the previous indentation level
	PrevLevel = 
		case get(indent) of 
			% process dictionary is empty, assume its 0
			undefined -> 0;
			L -> L
		end,

	% Put the current indent level in the process dictionary, called purely
	% for its side effects.
	put(indent, CurLevel),

	% Return the indentation or dedent
	scope_token(PrevLevel, CurLevel, Line).
	
% If the previous level > current level, emit one or more dedent tokens
scope_token(PrevLevel, CurLevel, Line) when PrevLevel > CurLevel ->
	HowMany = PrevLevel - CurLevel,
	{token, {dedent, HowMany, Line}};
% If the current level > the previous level, emit an indent token
scope_token(PrevLevel, CurLevel, Line) when CurLevel > PrevLevel ->
	{token, {indent, Line + 1}};
scope_token(PrevLevel, CurLevel, _Line) when CurLevel =:= PrevLevel ->
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

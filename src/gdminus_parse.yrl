Nonterminals expr Script Statement Statements
varDeclStmt constDecl enumDecl inheritance className keyValue array
functioncall constructorDecl ifStmt
array_items kv_items enum_list uminus unop arglist exprlist
.

Terminals 
number name string
% Operators
'+' '-' '*' '/' '(' ')' '='
% Logic
'false' 'true' '!' 'not'
% Keywords
var extends class_name const enum func indent dedent if else
% Other symbols
'.' ',' '[' ']' ':' '{' '}'
% Types
%int float null bool 
%String Vector2 Rect2 Vector3 Transform2D Plane Quat 
%AABB Basis Transform Color NodePath RID Object Array 
%Dictionary 
%%% Other
comparison
.

Rootsymbol Script.

%TODO
Right 100 '='.
Left 300 '+'.
Left 300 '-'.
Left 400 '*'.
Left 400 '/'.
Unary 1000 uminus.

Script -> Statements : '$1'.

Statements -> indent Statement dedent : [{indent, '$2', 'dedent'}].
Statements -> Statement : ['$1'].
Statements -> indent Statement Statements dedent : [{indent, '$2', 'dedent'}] ++ '$3'.
Statements -> Statement Statements : ['$1'] ++ '$2'.

Statement -> varDeclStmt     : '$1'.
Statement -> constDecl       : '$1'.
Statement -> enumDecl        : '$1'.
Statement -> constructorDecl : '$1'.
Statement -> inheritance     : '$1'.
Statement -> className       : '$1'.
Statement -> expr            : '$1'.
Statement -> ifStmt          : '$1'.
%Statement -> matchStmt      : '$1'.
%Statement -> assignmentStmt : '$1'.
%Unsupported: assert, yield, preload

expr -> keyValue : '$1'.
expr -> array : '$1'.
expr -> string : '$1'.
expr -> name : '$1'.
%expr -> functioncall : '$1'.
expr -> 'true' : '$1'.
expr -> 'false' : '$1'.
expr -> number : '$1'.
expr -> '(' expr ')' : '$2'.
expr -> expr '+' expr : {add, '$1', '$3'}.
expr -> expr '-' expr : {subtract, '$1', '$3'}.
expr -> expr '*' expr : {multiply, '$1', '$3'}.
expr -> expr '/' expr : {divde, '$1', '$3'}.
expr -> unop: '$1'.

unop -> uminus : {negation, '$1'}. 
unop -> 'not' expr : {negation, '$1'}.
unop -> '!' expr : {negation, '$1'}.

uminus -> '-' expr : {negation, '$1'}. 

array -> '[' ']' : [].
array -> '[' exprlist ']' : '$2'.

keyValue -> '{' kv_items '}' : '$2'.
kv_items -> expr ':' expr : [{kv,'$1','$3'}].
kv_items -> expr ':' expr ',' kv_items : '$5' ++ [{kv, '$1', '$3'}].

arglist -> '(' ')' : [].
arglist -> '(' exprlist ')' : '$2'.

exprlist -> expr : ['$1'] .
exprlist -> exprlist ',' expr : '$1' ++ ['$3'] .

varDeclStmt -> var name : {var,'$2'}.
varDeclStmt -> var name '=' expr : {var,'$2','$4'}.
% TODO/Maybe: Type hints 
%varDeclStmt -> var name ':' name : {var, '$2', {type, '$4'}}.
%varDeclStmt -> var name ':' name '=' expr : {var, '$2', '$6', {type, '$4'}}.
% TODO/Maybe: Inferred typing.
%varDeclStmt -> var name ':' '=' expr : {var, '$2', '$5'}.

constDecl -> const name '=' expr : {const, '$2', '$4'}.

%FIXME, not producing correct output
enumDecl -> enum '{' enum_list '}' : {enum, '$3'}.
enum_list -> name : ['$1'] .
enum_list -> name '=' expr : [{'$1','$3'}] .
enum_list -> enum_list ',' name : '$1' ++ ['$3'] .
enum_list -> enum_list ',' name '=' expr : '$1' ++ [{'$3','$5'}] .

inheritance -> extends string '.' name : {extends, '$2', '$4'}.
inheritance -> extends string : {extends, '$2'}.
inheritance -> extends name : {extends, '$2'}.

className -> class_name name ',' string : {class_name, '$2', '$4'}.
className -> class_name name : {class_name, '$2'}.

% Still introduces a shift conflict
%functioncall -> name arglist : {func_call, '$2'}.

constructorDecl -> 'func' name arglist ':' : {func_def, '$2', '$3'}.

ifStmt -> 'if' name comparison expr ':' : {ifStmt, '$3', '$2', '$4'}.
ifStmt -> 'else' ':' : {ifStmt, else}.

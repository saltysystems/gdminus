Nonterminals expr Script Statement Statements Suite
varDeclStmt constDecl enumDecl inheritance className keyValue array
functioncall constructorDecl
array_items kv_items enum_items uminus unop arglist arglist_items
.

Terminals 
number name string
% Operators
'+' '-' '*' '/' '(' ')' '='
% Logic
'false' 'true' '!' 'not'
% Keywords
var extends class_name const enum func indent dedent
% Other symbols
'.' ',' '[' ']' ':' '{' '}'
% Types
%int float null bool 
%String Vector2 Rect2 Vector3 Transform2D Plane Quat 
%AABB Basis Transform Color NodePath RID Object Array 
%Dictionary 
.

Rootsymbol Script.

%TODO
Right 100 '='.
Left 300 '+'.
Left 300 '-'.
Left 400 '*'.
Left 400 '/'.
Unary 1000 uminus.

Script -> Suite : '$1'.

Suite -> Statements: '$1'.
Suite -> indent Statements dedent : '$1'.

Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'] ++ '$2'.

Statement -> varDeclStmt     : '$1'.
Statement -> constDecl       : '$1'.
Statement -> enumDecl        : '$1'.
Statement -> constructorDecl : '$1'.
Statement -> inheritance     : '$1'.
Statement -> className       : '$1'.
Statement -> expr            : '$1'.
%Statement -> ifStmt         : '$1'.
%Statement -> matchStmt      : '$1'.
%Statement -> assignmentStmt : '$1'.
%Unsupported: assert, yield, preload

expr -> keyValue : '$1'.
expr -> array : '$1'.
expr -> string : '$1'.
expr -> name : '$1'.
expr -> functioncall : '$1'.
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

array -> '[' array_items ']' : '$2'.
array_items -> expr : ['$1'].
array_items -> expr ',' array_items : ['$1'] ++ '$2'.

%TODO: Rework expr to properly capture items such as function calls, strings,
%      named identifiers, etc
keyValue -> '{' kv_items '}' : '$2'.
kv_items -> expr ':' expr : ['$1'].
kv_items -> expr ':' expr ',' kv_items: ['$1'] ++ '$2'.

arglist -> '(' arglist_items ')' : '$2'.
arglist_items -> expr : ['$1'].
arglist_items -> expr ',' arglist_items : ['$1'] ++ '$2'.

varDeclStmt -> var name : {var,'$2'}.
varDeclStmt -> var name '=' expr : {var,'$2','$4'}.
% TODO/Maybe: Type hints 
%varDeclStmt -> var name ':' name : {var, '$2', {type, '$4'}}.
%varDeclStmt -> var name ':' name '=' expr : {var, '$2', '$6', {type, '$4'}}.
% TODO/Maybe: Inferred typing.
%varDeclStmt -> var name ':' '=' expr : {var, '$2', '$5'}.

constDecl -> const name '=' expr : {const, '$2', '$4'}.

enumDecl -> enum '{' enum_items '}' : '$1'.
enumDecl -> enum name '{' enum_items '}' : '$1'.
enum_items -> name : ['$1'].
enum_items -> name '=' number : [{enum, '$1', $2}].
enum_items -> name '=' '-' number : [{enum, '$1', $2}].
enum_items -> name ',' enum_items : ['$1'] ++ '$2'.

inheritance -> extends string '.' name : {extends, '$2', '$4'}.
inheritance -> extends string : {extends, '$2'}.
inheritance -> extends name : {extends, '$2'}.

className -> class_name name ',' string : {class_name, '$2', '$4'}.
className -> class_name name : {class_name, '$2'}.

% Still introduces a shift conflict
functioncall -> name arglist : {func_call, '$2'}.

constructorDecl -> 'func' name arglist ':' : {func_def, '$2'}.

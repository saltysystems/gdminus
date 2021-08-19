Expect 2.

Nonterminals 
expr Script Statement Statements
varDeclStmt constDecl enumDecl inheritance className keyValue array
functioncall constructorDecl ifStmt assignmentStmt
forStmt returnStmt whileStmt breakStmt continueStmt
kv_items enum_list uminus unop arglist exprlist
Block matchStmt matchcondition matchconditions pname
.

Terminals 
number name string
% Operators
'+' '-' '*' '/' '(' ')' '='
% Logic
'false' 'true' '!' 'not' 'is' '==' '>=' '<=' '!=' '>' '<'
% Keywords
var extends class_name const enum func indent dedent if else elif
for in return while break continue match
% Other symbols
'.' ',' '[' ']' ':' '{' '}'
% Types
%int float null bool 
%String Vector2 Rect2 Vector3 Transform2D Plane Quat 
%AABB Basis Transform Color NodePath RID Object Array 
%Dictionary 
%%% Other
.

Rootsymbol Script.

%TODO
Right 100 '='.
Left 300 '+' '-'.
Left 400 '*' '/'.
Left 500 'is' '==' '<' '>' '<=' '>=' '!='.
Unary 1000 uminus.

Script -> Statements : '$1'.

Statements -> Statement : ['$1'].
Statements -> Statements Statement : '$1' ++ ['$2'].

Block -> indent Statements dedent : '$2'.
Block -> indent returnStmt dedent : ['$2'].
Block -> indent Statements returnStmt dedent : '$2' ++ ['$3'].

Statement -> varDeclStmt     : '$1'.
Statement -> constDecl       : '$1'.
Statement -> enumDecl        : '$1'.
Statement -> constructorDecl : '$1'.
Statement -> inheritance     : '$1'.
Statement -> className       : '$1'.
Statement -> expr            : '$1'.
Statement -> ifStmt          : '$1'.
Statement -> breakStmt       : '$1'.
Statement -> continueStmt    : '$1'.
Statement -> matchStmt       : '$1'.
Statement -> assignmentStmt  : '$1'.
Statement -> forStmt         : '$1'.
Statement -> whileStmt       : '$1'.
%Unsupported: assert, yield, preload

expr -> keyValue : '$1'.
expr -> array : '$1'.
expr -> string : '$1'.
expr -> functioncall : '$1'.
expr -> 'true' : '$1'.
expr -> 'false' : '$1'.
expr -> number : '$1'.
expr -> '(' expr ')' : '$2'.
expr -> pname : '$1'.
expr -> expr '+' expr  : {'+', '$1', '$3'}.
expr -> expr '-' expr  : {'-', '$1', '$3'}.
expr -> expr '*' expr  : {'*', '$1', '$3'}.
expr -> expr '/' expr  : {'/', '$1', '$3'}.
expr -> expr '==' expr : {'==', '$1', '$3'}.
expr -> expr is expr : {'==', '$1', '$3'}.
expr -> expr '!=' expr : {'!=', '$1', '$3'}.
expr -> expr '>=' expr : {'>=', '$1', '$3'}.
expr -> expr '<=' expr : {'<=', '$1', '$3'}.
expr -> expr '<' expr  : {'<', '$1', '$3'}.
expr -> expr '>' expr  : {'>', '$1', '$3'}.
expr -> unop: '$1'.
expr -> uminus: '$1'.

unop -> 'not' expr : {negation, '$2'}.
unop -> '!' expr : {negation, '$2'}.

uminus -> '-' expr : {negation, '$2'}. 

pname -> name '.' name : {'$1', to_string('$3')}.
pname -> name : '$1'.

array -> '[' ']' : [].
% Handles the case where the author indents everything inside the braces but not the braces themselves
array -> '[' indent exprlist dedent ']' : '$3'.
% Handles the case where the author indents the tailing brace and everything inside the braces
array -> '[' indent exprlist ']' dedent : '$3'.
array -> '[' exprlist ']' : '$2'.

keyValue -> '{' '}' : {}.
keyValue -> '{' kv_items '}' : '$2'.
% Handles the case where the author indents everything inside the braces but not the braces themselves
keyValue -> '{' indent kv_items dedent '}' : '$3'.
% Handles the case where the author indents the tailing brace and everything inside the braces
keyValue -> '{' indent kv_items '}' dedent : '$3'.

kv_items -> expr ':' expr : [{kv,'$1','$3'}].
kv_items -> expr ':' expr ',' kv_items : [{kv, '$1', '$3'}] ++ '$5'.

arglist -> '(' ')' : [].
arglist -> '(' exprlist ')' : '$2'.

exprlist -> expr : ['$1'] .
exprlist -> exprlist ',' expr : '$1' ++ ['$3'] .

varDeclStmt -> var pname : {var,'$2'}.
varDeclStmt -> var pname '=' expr : {var,'$2','$4'}.

assignmentStmt -> pname '=' expr : {'=', '$1', '$3'}.

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

className -> class_name name ',' string : {class_name, '$2', '$4' }.
className -> class_name name : {class_name, '$2' }.

% Still introduces a shift conflict, which we are suppressing
% minimum amount of glue needed to support some of the godot native functions e.g. OS.<blah> 
functioncall -> pname arglist : {func_call, '$1', '$2'}.

constructorDecl -> 'func' name arglist ':' Block : {func, '$2', '$3', '$5'}.

forStmt -> 'for' name 'in' expr ':' Block : {for, '$2', '$4', '$6'}.

whileStmt -> while expr ':' Block : {while, '$2', '$4'}.

breakStmt -> break : {break}.
continueStmt -> continue : {continue}.

matchStmt -> match expr ':' indent matchconditions dedent : {match, '$2', '$5'}.

matchconditions -> matchcondition : ['$1'].
matchconditions -> matchconditions matchcondition : '$1' ++ ['$2'].

matchcondition -> expr ':' Block : {match_cond, '$1', '$3'}. 

returnStmt -> return : {return}.
returnStmt -> return expr : {return, '$2'}.

ifStmt -> 'if' expr ':' Block : {'if', '$2', '$4'}.
ifStmt -> 'elif' expr ':' Block : {elif, '$2', '$4'}.
ifStmt -> 'else' ':' Block : {else, '$3'}.

Erlang code.
% Convert a type to a string. e.g., accessing dictionary values via the dot
% operator such as foo.bar is equal to foo["bar"]
to_string({name, Line, Val}) ->
    {string, Line, Val}.


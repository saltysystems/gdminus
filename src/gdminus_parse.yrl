Nonterminals Expression Term Factor Script Statement Statements 
varDeclStmt inheritance className keyValue list
functioncall list_items kv_items
.

Terminals 
number name string
% Operators
'+' '-' '*' '/' '(' ')' '='
% Keywords
var extends class_name
% Other symbols
'.' ',' '[' ']' ':' '{' '}'
.

Rootsymbol Script.

Script -> Statements : '$1'.

Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'] ++ '$2'.

Statement -> varDeclStmt : '$1'.
Statement -> inheritance : '$1'.
Statement -> className   : '$1'.
Statement -> Expression  : '$1'.

Expression -> Expression '+' Term : {'+', '$1', '$3'}.
Expression -> Expression '-' Term : {'-', '$1', '$3'}.
Expression -> Term : '$1'.

Term -> Factor '*' Term : {'*', '$1', '$3'}.
Term -> Factor '/' Term : {'/', '$1', '$3'}.
Term -> Factor : '$1'.

Factor -> '(' Expression ')': '$2'.
Factor -> number : '$1'.

list -> '[' list_items ']' : '$2'.
list_items -> Expression : ['$1'].
list_items -> Expression ',' list_items : ['$1'] ++ '$2'.

%TODO: Rework Expression to properly capture items such as function calls, strings, named identifiers, etc
keyValue -> '{' kv_items '}' : '$2'.
kv_items -> Expression ':' Expression : ['$1'].
kv_items -> Expression ':' Expression ',' kv_items: ['$1'] ++ '$2'.

varDeclStmt -> var name : {var,'$2'}.
varDeclStmt -> var name '=' string : {var,'$2','$4'}.
varDeclStmt -> var name '=' Expression : {var,'$2','$4'}.
varDeclStmt -> var name '=' list : {var, '$2', '$4' }.
varDeclStmt -> var name '=' '{' keyValue '}' : {var, '$2', '$4' }.

inheritance -> extends string '.' name : {extends, '$2', '$4'}.
inheritance -> extends string : {extends, '$2'}.
inheritance -> extends name : {extends, '$2'}.

className -> class_name name ',' string : {class_name, '$2', '$4'}.
className -> class_name name : {class_name, '$2'}.

functioncall -> name '(' ')' : {func, '$3'}.

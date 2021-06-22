Nonterminals Expression Term Factor Script Statement Statements varDeclStmt.

Terminals 
number name
% Operators
'+' '-' '*' '/' '(' ')' '='
% Keywords
var.

Rootsymbol Script.

Script -> Statements : '$1'.

Statements -> Statement : ['$1'].
Statements -> Statement Statements : ['$1'] ++ '$2'.

Statement -> varDeclStmt : '$1'.
Statement -> Expression : '$1'.

Expression -> Expression '+' Term : {'+', '$1', '$3'}.
Expression -> Expression '-' Term : {'-', '$1', '$3'}.
Expression -> Term : '$1'.

Term -> Factor '*' Term : {'*', '$1', '$3'}.
Term -> Factor '/' Term : {'/', '$1', '$3'}.
Term -> Factor : '$1'.

Factor -> '(' Expression ')': '$2'.
Factor -> number : '$1'.

varDeclStmt -> var name : {var,'$2'}.
varDeclStmt -> var name '=' Expression : {var,'$2','$4'}.

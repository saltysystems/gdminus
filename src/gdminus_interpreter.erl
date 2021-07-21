-module(gdminus_interpreter).
% Simple AST treewalker for GDMinus

-export([walk/1]).

walk(Tree) ->
    walk(Tree, maps:new()).

walk([], St0) ->
    io:format("End of program~n"),
    io:format("Final state: ~p~n", [St0]);
walk([{Oper, Val1, Val2} | Rest], St0) when Oper == '+'; Oper == '-'; Oper =='*'; Oper == '/' ->
    R = exp({Oper, Val1,Val2}),
    io:format("Output: ~p~n", [R]),
    walk(Rest, St0);
walk([{var, Name} | Rest], St0) ->
    St1 = declStmt({var, Name, null}, St0),
    io:format("Output: ~p~n", [St1]),
    walk(Rest, St1);
walk([{var, Name, Val} | Rest], St0) ->
    St1 = declStmt({var, Name, Val}, St0),
    io:format("Output: ~p~n", [St1]),
    walk(Rest, St1);
walk([{'=', Name, Val} | Rest], St0) ->
    St1 = assignStmt({'=', Name, Val}, St0),
    io:format("Output: ~p~n", [St1]),
    walk(Rest, St1).

exp({string, _Line, Val}) ->
    Val;
exp({number, _Line, Val}) ->
    Val;
exp({'+', Val1, Val2}) ->
    overload_add(exp(Val1),exp(Val2));
exp({'-', Val1, Val2}) ->
    exp(Val1) - exp(Val2);
exp({'*', Val1, Val2}) ->
    exp(Val1) * exp(Val2);
exp({'/', Val1, Val2}) ->
    exp(Val1) / exp(Val2).

overload_add(Val1, Val2) when is_number(Val1), is_number(Val2) ->
    Val1 + Val2;
overload_add(Val1, Val2) when is_list(Val1), is_list(Val2) ->
    Val1 ++ Val2.

declStmt({var, {name, _L, Name}, Val}, St0) ->
    case maps:is_key(Name, St0) of
        false ->
            % Only accept if it doesn't exist already
            maps:put(Name, Val, St0)
    end.
assignStmt({'=', {name, _L, Name}, Val}, St0) ->
    case maps:is_key(Name, St0) of
        true ->
            % Only allow assignment if the variable has been declared
            maps:put(Name, Val, St0)
    end.

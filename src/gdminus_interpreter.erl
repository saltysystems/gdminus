-module(gdminus_interpreter).
% Simple AST treewalker for GDMinus

-export([walk/1]).

-record(gdm_env, {id=0, enclosing=0, map=maps:new()}).
-record(gdm_state, {curEnv=0, envs}).

walk(Tree) ->
    E0 = #gdm_env{id=0, enclosing=0, map=maps:new()},
    St0 = #gdm_state{curEnv=0, envs=[E0]},
    walk(Tree, St0).

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

% Provide two ways of using the '+' operator, adding numbers and concatinating strings.
overload_add(Val1, Val2) when is_number(Val1), is_number(Val2) ->
    Val1 + Val2;
overload_add(Val1, Val2) when is_list(Val1), is_list(Val2) ->
    Val1 ++ Val2.

% We can declare only if the variable IS NOT declared in the scope or any preceeding scope
declStmt({var, {name, _L, Name}, Val}, St0) ->
    case maps:is_key(Name, St0) of
        false ->
            % Only accept if it doesn't exist already
            maps:put(Name, Val, St0)
    end.
% We only assign if the variable IS declared in the scope or any preceeding scope
assignStmt({'=', {name, _L, Name}, Val}, St0) ->
    case maps:is_key(Name, St0) of
        true ->
            % Only allow assignment if the variable has been declared
            maps:put(Name, Val, St0)
    end.

% Traverse the environment tree to look for the key wanted. TODO: TEST!
walk_env(Key, St0) ->
    Results = walk_env(Key, St0, []),
    % Logically OR the results
    lists:foldl(fun(X,Y) -> X or Y end, false, Results).
walk_env(Key, #gdm_state{curEnv=0, envs=Envs}, Acc) -> 
    % Current environment is 0, the global scope, should always succeed
    E = lists:keyfind(0, #gdm_env.id, Envs),
    Map = E#gdm_env.map,
    [ maps:is_key(Key, Map) | Acc ];
walk_env(Key, St0 = #gdm_state{curEnv=Level, envs=Envs}, Acc) -> 
    E = lists:keyfind(0, #gdm_env.id, Envs),
    Map = E#gdm_env.map,
    NewAcc = [ maps:is_key(Key, Map) | Acc ],
    % rather inefficient since we can just halt when we find the boolean we want. TODO fix
    walk_env(Key, St0#gdm_state{curEnv=Level - 1, envs=Envs}, NewAcc).

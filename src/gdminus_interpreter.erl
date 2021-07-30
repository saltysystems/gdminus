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
    St0;
walk([{Oper, Val1, Val2} | Rest], St0) when 
      Oper == '+'; Oper == '-'; Oper =='*'; Oper == '/'; 
      Oper == '=='; Oper == '>='; Oper == '<='; 
      Oper == '!='; Oper == '>' ; Oper == '<' ->
    R = exp({Oper, Val1,Val2}, St0),
    io:format("Output: ~p~n", [R]),
    walk(Rest, St0);
walk([{var, Name} | Rest], St0) ->
    St1 = declStmt({var, Name, null}, St0),
    io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{var, Name, Val} | Rest], St0) ->
    St1 = declStmt({var, Name, Val}, St0),
    io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{'=', Name, Val} | Rest], St0) ->
    St1 = assignStmt({'=', Name, Val}, St0),
    io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{Oper, Exp, Block} | Rest], St0) when Oper == 'if'; Oper == 'elif' ->
    St1 = ifStmt(exp(Exp, St0), Block, Rest, St0),
    io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1).

exp({name, _Line, Val}, St0) ->
    get_env(Val, St0);
exp({string, _Line, Val}, _St0) ->
    Val;
exp({number, _Line, Val}, _St0) ->
    Val;
exp({'+', Val1, Val2}, St0) ->
    overload_add(exp(Val1,St0),exp(Val2,St0));
exp({'-', Val1, Val2}, St0) ->
    exp(Val1, St0) - exp(Val2, St0);
exp({'*', Val1, Val2}, St0) ->
    exp(Val1, St0) * exp(Val2, St0);
exp({'/', Val1, Val2}, St0) ->
    exp(Val1, St0) / exp(Val2, St0);
exp({'==', Val1, Val2}, St0) ->
    exp(Val1, St0) == exp(Val2, St0);
exp({'!=', Val1, Val2}, St0) ->
    exp(Val1,St0) /= exp(Val2, St0);
exp({'>=', Val1, Val2}, St0) ->
    exp(Val1, St0) >= exp(Val2, St0);
exp({'<=', Val1, Val2}, St0) ->
    exp(Val1, St0) =< exp(Val2, St0);
exp({'>', Val1, Val2}, St0) ->
    exp(Val1, St0) > exp(Val2, St0);
exp({'<', Val1, Val2}, St0) ->
    exp(Val1, St0) < exp(Val2, St0).

% Provide two ways of using the '+' operator, adding numbers and concatinating strings.
overload_add(Val1, Val2) when is_number(Val1), is_number(Val2) ->
    Val1 + Val2;
overload_add(Val1, Val2) when is_list(Val1), is_list(Val2) ->
    Val1 ++ Val2.

% We can declare only if the variable IS NOT declared in the scope or any preceeding scope
declStmt({var, {name, _L, Name}, Val}, St0) ->
    case walk_env(Name, St0, false) of
        {false, Where} ->
            % Only accept if it doesn't exist already
            io:format("(Decl) Going to put ~p on level ~p~n", [Name, Where]),
            put_env2(Name, Val, Where, St0)
    end.
% We only assign if the variable IS declared in the scope or any preceeding scope
assignStmt({'=', {name, _L, Name}, Val}, St0) ->
    case walk_env(Name, St0, true) of
        {true, Where} ->
            % Only allow assignment if the variable has been declared
            io:format("(Decl) Going to update ~p on level ~p~n", [Name, Where]),
            put_env2(Name, Val, Where, St0)
    end.


ifStmt(false, _Block, [{elif, Expr2, Block2}|Rest], St0) ->
    % the if statement didn't evaluate, so this branch never executes
    ifStmt(exp(Expr2, St0), Block2, Rest, St0);
ifStmt(false, _Block, [{else, Block2}|_Rest], St0) ->
    L = St0#gdm_state.curEnv,
    % However, we do have an "else", so make sure we evaluate that
    eval_block(Block2, St0#gdm_state{curEnv=L+1});
ifStmt(false, _Block, _, St0) ->
    % Bare false statement, no state change.
    St0;
ifStmt(true, Block, _, St0) ->
    L = St0#gdm_state.curEnv,
    eval_block(Block, St0#gdm_state{curEnv=L+1}).


eval_block(Block, St0) ->
    CurEnv = St0#gdm_state.curEnv,
    io:format("Begin evaluating block at level ~p~n", [CurEnv]),
    walk(Block, St0#gdm_state{curEnv=CurEnv}).

% Put Env 2 will put stuff exactly whre it's told to.
put_env2(Key, Val, Where, St0) ->
    % Assume the environment is setup and get the current Env list
    Envs0 = St0#gdm_state.envs,
    io:format("Current envs are: ~p~n", [Envs0]),
    % "Where" is the level where we wnat to do a replacement, so get the
    % environment for that level or make it if it doesn't exist
    Envs1 = 
        case lists:keyfind(Where, #gdm_env.id, Envs0) of
            % have to make a new env
            false ->
                % special casing around level 0 (global scope)
                if 
                    Where == 0 ->
                        E = #gdm_env{};
                    true ->
                        E = #gdm_env{id=Where, enclosing=Where-1}
                end,
                M0 = E#gdm_env.map,
                % New map has the value we want inserted
                M1 = maps:put(Key, Val, M0),
                % Reinsert the map into the environment
                E1 = E#gdm_env{map=M1},
                [ E1 | Envs0 ];
            % Found an env
            E0 -> 
                M0 = E0#gdm_env.map,
                % New map has the value we want inserted
                M1 = maps:put(Key, Val, M0),
                % Reinsert the map into the environment
                E1 = E0#gdm_env{map=M1},
                % Reinsert the environment into the EnvList
                lists:keyreplace(Where, #gdm_env.id, Envs0, E1)
        end,
    % Update the state (phew!)
    St1 = St0#gdm_state{envs=Envs1},
    St1.

put_env(Key, Val, St0 = #gdm_state{curEnv=CurEnv, envs=Envs}) ->
    % First check to see if the environment exists already in the list of
    % environments. 
    NewEnvList = case lists:keyfind(CurEnv, #gdm_env.id, Envs) of
            % If it doesn't, make a new map for this level.
            false ->
                io:format("No existing map at level ~p~n",[CurEnv]),
                % If we're at level 0, the global scope, then our enclosing
                % level is ourself.
                if 
                    CurEnv == 0 ->
                        G0 = #gdm_env{};
                    true ->
                        % must it be CurEnv - 1 ?
                        G0 = #gdm_env{id=CurEnv, enclosing=CurEnv-1}
                end,
                % Update the map that we have with the new key/val
                M1 = maps:put(Key, Val, G0#gdm_env.map),
                % Update the environment with the new map
                G1 = G0#gdm_env{map=M1},
                % We know that the environment didn't have this level before so
                % just add it to the existing list
                [ G1 | Envs ];
            Tuple ->
                % Update the map that we have with the new key/val
                M1 = maps:put(Key, Val, Tuple#gdm_env.map),
                % Update the environment with the new map
                G1 = Tuple#gdm_env{map=M1},
                % Replace the existing env state with our new fancy pants
                % version
                lists:keyreplace(CurEnv, #gdm_env.id, Envs, G1)
        end,
    io:format("new Env list is: ~p~n", [NewEnvList]),
    St0#gdm_state{envs=NewEnvList}.


% Traverse the environment tree to look for the key wanted. TODO: TEST!
%walk_env(Key, St0, LookingFor) ->
%    Results = walk_env(Key, St0, []),
%    % where?
%    lists:foldl(fun(X,Y) -> X or Y end, false, Results).
walk_env(Key, #gdm_state{curEnv=0, envs=Envs}, LookingFor) -> 
    % Current environment is 0, the global scope, should always succeed
    E = lists:keyfind(0, #gdm_env.id, Envs),
    Map = E#gdm_env.map,
    Result = maps:is_key(Key, Map),
    if
        LookingFor == Result ->
            {Result, 0};
        true ->
            false % can't find the thing we want
    end;
walk_env(Key, St0 = #gdm_state{curEnv=Level, envs=Envs}, LookingFor) -> 
    E = case lists:keyfind(Level, #gdm_env.id, Envs) of
        false ->
            #gdm_env{id=Level, enclosing=Level-1};
        Tuple ->
            Tuple
    end,
    Map = E#gdm_env.map,
    %NewAcc = [ maps:is_key(Key, Map) | Acc ],
    Result = maps:is_key(Key, Map),
    if
        LookingFor == Result ->
            {Result, Level};
        true ->
            walk_env(Key, St0#gdm_state{curEnv=Level - 1, envs=Envs}, LookingFor)
    end.
    % rather inefficient since we can just halt when we find the boolean we
    % want.
    %walk_env(Key, St0#gdm_state{curEnv=Level - 1, envs=Envs}, NewAcc).

get_env(Key, St0 = #gdm_state{curEnv=0, envs=Envs}) -> 
    % Current environment is 0, the global scope, should always succeed
    io:format("Getting env at level ~p for key ~p~n",[0,Key]),
    E = maybe_keyfind_env(0, Envs),
    Map = E#gdm_env.map,
    case maps:get(Key, Map, null) of
        null ->
            io:format("Key ~p wasn't found, returning null~n", [Key]),
            null; %undefined, something has probably gone wrong
        A ->
            io:format("Value of A is ~p~n", [A]),
            exp(A, St0)
            %exp(Val, St0)
    end;
get_env(Key, St0 = #gdm_state{curEnv=Level, envs=Envs}) -> 
    io:format("Getting env at level ~p for key ~p~n",[Level,Key]),
    E = maybe_keyfind_env(Level, Envs),
    Map = E#gdm_env.map,
    case maps:get(Key, Map, null) of
        null ->
            % Recurse
            io:format("Key ~p wasn't found, check ~p~n", [Key, Level-1]),
            get_env(Key, St0#gdm_state{curEnv=Level - 1, envs=Envs});
        A ->
            io:format("Value of A is ~p~n", [A]),
            exp(A, St0)
            %exp(Val, St0)
    end.

maybe_keyfind_env(Level, Envs) ->
   case lists:keyfind(Level, #gdm_env.id, Envs) of
        false ->
            #gdm_env{};
        Tuple ->
            Tuple
   end.


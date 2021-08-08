-module(gdminus_interpreter).
% Simple AST treewalker for GDMinus
-include_lib("eunit/include/eunit.hrl").

-export([walk/1]).

-record(gdm_env, {id=0, vars=maps:new(), functions=maps:new()}).
-record(gdm_state, {curEnv=0, curLoop=0, envs, breakers=maps:new()}).

walk(Tree) ->
    E0 = #gdm_env{id=0, vars=maps:new(), functions=maps:new()},
    St0 = #gdm_state{curEnv=0, curLoop=0, envs=[E0]},
    walk(Tree, St0).

walk([], St0) ->
    St0;
walk(_, St0) when map_size(St0#gdm_state.breakers) > 0 ->
    % any time we see a breaker, deal with it.
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
    %io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{var, Name, Val} | Rest], St0) ->
    St1 = declStmt({var, Name, Val}, St0),
    %io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{'=', Name, Val} | Rest], St0) ->
    St1 = assignStmt({'=', Name, Val}, St0),
    %io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{Oper, Exp, Block} | Rest], St0) when Oper == 'if'; Oper == 'elif' ->
    E = St0#gdm_state.curEnv,
    St1 = ifStmt(exp(Exp, St0), Block, Rest, St0#gdm_state{curEnv = E + 1}),
    %io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{while, Exp, Block} | Rest], St0) ->
    L = St0#gdm_state.curLoop,
    St1 = whileStmt(Exp, Block, St0#gdm_state{curLoop = L + 1}),
    %io:format("Next state: ~p~n", [St1]),
    walk(Rest, St1);
walk([{for, {name, _Line, Name}, Iter, Block} | Rest], St0) ->
    L = St0#gdm_state.curLoop,
    St1 = forStmt(Name, exp(Iter, St0), Block, St0#gdm_state{curLoop = L + 1}),
    walk(Rest, St1);
walk([{func, {name, _Line, Name}, Args, Block} | Rest], St0) ->
    St1 = constructorStmt(Name, Args, Block, St0),
    walk(Rest, St1);
walk([{func_call, {name, _Line, Name}, Args} | Rest], St0) ->
    St1 = exp({func_call, Name, Args}, St0),
    walk(Rest, St1);
walk([{Oper} | _Rest], St0) when Oper == 'break'; Oper == 'continue' ->
    % check the state loop so we die with an error  when we're not in for/while
    CurLoop = St0#gdm_state.curLoop,
    case CurLoop >= 1 of
        true ->
            Breakers = St0#gdm_state.breakers,
            %io:format("(walk) Caught break. Current state: ~p~n", [St0]),
            St1 = St0#gdm_state{breakers=maps:put(CurLoop, Oper, Breakers)},
            St1
    end.

exp({string, _Line, Val}, _St0) ->
    Val;
exp({name, _Line, Val}, St0) ->
    get_var(Val, St0);
exp({number, _Line, Val}, _St0) ->
    Val;
exp({func_call, Name, Args}, St0) ->
    call(Name, Args, St0);
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
    exp(Val1, St0) < exp(Val2, St0);
exp(Other, _St0) ->
    Other.

% Provide two ways of using the '+' operator, adding numbers and concatinating strings.
overload_add(Val1, Val2) when is_number(Val1), is_number(Val2) ->
    Val1 + Val2;
overload_add(Val1, Val2) when is_list(Val1), is_list(Val2) ->
    Val1 ++ Val2.

call(Name, Args, St0) ->
    % Retrieve the parameters function definition
    {Parameters, FunBlock} = get_fun(Name, St0),
    % Make sure the arguments and the params have the same arity
    case length(Parameters) == length(Args) of
        true ->
            E = St0#gdm_state.curEnv,
            St1 = St0#gdm_state{curEnv = E + 1},
            %A = [ declStmt({var, {name, null, ParamName}, Args}, St0) || 
            %                {Args, {_ParamType, _L2, ParamName}}  <- lists:zip(Args, Parameters) ],
            L = lists:zip(Args, Parameters)
            %TODO:
            % 1. Declare every variable in the parameter list with the values supplied from the arglist
            % 2. (Warn if any variables are shadowed (maybe?). )
            % 3. Process the block with the temp vars
            % 4. (Maybe? Do not allow break/continue functions to be called here)
    end.
            
            
    
    

% We can declare only if the variable IS NOT declared in the scope or any preceeding scope
declStmt({var, {name, _L, Name}, Val}, St0) ->
    case walk_var(Name, St0, false) of
        {false, Where} ->
            % Only accept if it doesn't exist already
            %io:format("(Decl) Going to put ~p on level ~p~n", [Name, Where]),
            put_var(Name, Val, Where, St0)
    end.
% We only assign if the variable IS declared in the scope or any preceeding scope
assignStmt({'=', {name, _L, Name}, Val}, St0) ->
    case walk_var(Name, St0, true) of
        {true, Where} ->
            % Only allow assignment if the variable has been declared
            %io:format("(Decl) Going to update ~p on level ~p~n", [Name, Where]),
            put_var(Name, Val, Where, St0)
    end.
% Only allow functions to be declared at the outermost scope
constructorStmt(Name, Args, Block, St0 = #gdm_state{} ) when St0#gdm_state.curEnv == 0 ->
    case walk_fun(Name, St0, false) of
        {false, Where} ->
            % Only accept if it doesn't exist already
            put_fun(Name, {Args,Block}, Where, St0)
    end.

ifStmt(false, _Block, [{elif, Expr2, Block2}|Rest], St0) ->
    ifStmt(exp(Expr2, St0), Block2, Rest, St0);
ifStmt(false, _Block, [{else, Block2}|_Rest], St0) ->
    eval_block(Block2, St0);
ifStmt(false, _Block, _, St0) ->
    % Bare false statement, no block evaluation, so decrement
    E = St0#gdm_state.curEnv,
    St0#gdm_state{curEnv = E - 1};
ifStmt(true, Block, _, St0) ->
    eval_block(Block, St0).

forStmt(Name,Iter,Block,St0) when is_integer(Iter) ->
    R = gdlist_seq(1, Iter),
    io:format("Current vals: ~p,~p~n", [Name, R]),
    forStmt(Name, R, Block, St0);
forStmt(Name,[],_Block, St0) ->
    io:format("Current state: ~p~n", [St0]),
    St0;
forStmt(Name,Iter,Block, St0) when is_list(Iter) ->
    [{_Type, _Line, Num}|Tail] = Iter,
    Where = St0#gdm_state.curEnv+1, % the variabble Name is implicitly declared in the block
    St1 = put_var(Name, Num, Where, St0),
    io:format("Current state: ~p~n", [St1]),
    E = St1#gdm_state.curEnv,
    St2 = eval_block(Block, St1#gdm_state{curEnv=E+1}),
    forStmt(Name, Tail, Block, St2).

whileStmt(Exp,Block,St0) ->
    io:format("Current state: ~p~n", [St0]),
    whileStmt(exp(Exp, St0), Exp, Block, St0).
whileStmt(false, _Exp, _Block, St0) ->
    L = St0#gdm_state.curLoop,
    St0#gdm_state{curLoop = L - 1}; % loop is finished, decrement the loop level
whileStmt(true, Exp, Block, St0) ->
    E = St0#gdm_state.curEnv,
    St1 = eval_block(Block, St0#gdm_state{curEnv=E+1}),
    CurLoop = St0#gdm_state.curLoop,
    Breakers = St0#gdm_state.breakers,
    % Have to check if a break is introduced here because we need to know if
    % the While statement should be stopped early.
    case maps:get(CurLoop, Breakers, false) of
        false -> 
            % no c-c-combo breaker so far, proceed as normal
            whileStmt(Exp, Block, St1);
        break ->
            % Stop the recursion and return the current state
            St2 = handle_breaker(St1),
            St2#gdm_state{curLoop = CurLoop - 1};
        continue ->
            % Handle the breaker, and then run the next phase of the loop
            St2 = handle_breaker(St1),
            io:format("Current state: ~p~n", [St2]),
            whileStmt(Exp, Block, St2)
    end.

eval_block(Block, St0) ->
    CurEnv = St0#gdm_state.curEnv,
    CurLoop = St0#gdm_state.curLoop,
    Breakers = St0#gdm_state.breakers,
    Envs = St0#gdm_state.envs,
    St1 = case maps:get(CurLoop, Breakers, false) of 
              false ->
                 % io:format("About to evaluate block ~p~n", [Block]),
                  walk(Block, St0#gdm_state{curEnv=CurEnv});
              break ->
                  io:format("(eval) Caught break. Current state: ~p~n", [St0]),
                  % Just return whatever we have so far
                  St0;
              continue ->
                  io:format("(eval) Caught continue. Current state ~p~n", [St0]),
                  St0
          end,
    % Block is evaluated so we can decrement env and purge this env we created
    NewEnvs = lists:keydelete(CurEnv, #gdm_env.id, Envs),
    St1#gdm_state{curEnv = CurEnv - 1, envs=NewEnvs}.

handle_breaker(St0) ->
    CurLoop = St0#gdm_state.curLoop,
    Breakers = St0#gdm_state.breakers,
    St0#gdm_state{breakers=maps:remove(CurLoop, Breakers)}.

%TODO : expand for non-numbers ?
gdlist_seq(From, To) ->
    A = lists:seq(From, To),
    [ {number, null, N} || N <- A ].

put_var(Key, Val, Where, St0) ->
    % Assume the environment is setup and get the current Env list
    Envs0 = St0#gdm_state.envs,
    %io:format("Current envs are: ~p~n", [Envs0]),
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
                        E = #gdm_env{id=Where}
                end,
                M0 = E#gdm_env.vars,
                % New map has the value we want inserted
                % Try to evaluate the expression as much as possible
                M1 = maps:put(Key, exp(Val, St0), M0),
                % Reinsert the map into the environment
                E1 = E#gdm_env{vars=M1},
                [ E1 | Envs0 ];
            % Found an env
            E0 -> 
                M0 = E0#gdm_env.vars,
                % New map has the value we want inserted
                % Try to evaluate the expression as much as possible
                M1 = maps:put(Key, exp(Val, St0), M0),
                % Reinsert the map into the environment
                E1 = E0#gdm_env{vars=M1},
                % Reinsert the environment into the EnvList
                lists:keyreplace(Where, #gdm_env.id, Envs0, E1)
        end,
    % Update the state (phew!)
    St1 = St0#gdm_state{envs=Envs1},
    St1.

walk_var(Key, #gdm_state{curEnv=0, envs=Envs}, LookingFor) -> 
    % Current environment is 0, the global scope, should always succeed
    E = lists:keyfind(0, #gdm_env.id, Envs),
    Map = E#gdm_env.vars,
    Result = maps:is_key(Key, Map),
    if
        LookingFor == Result ->
            {Result, 0};
        true ->
            false % can't find the thing we want
    end;
walk_var(Key, St0 = #gdm_state{curEnv=Level, envs=Envs}, LookingFor) -> 
    E = case lists:keyfind(Level, #gdm_env.id, Envs) of
        false ->
            #gdm_env{id=Level};
        Tuple ->
            Tuple
    end,
    Map = E#gdm_env.vars,
    %NewAcc = [ maps:is_key(Key, Map) | Acc ],
    Result = maps:is_key(Key, Map),
    if
        LookingFor == Result ->
            {Result, Level};
        true ->
            walk_var(Key, St0#gdm_state{curEnv=Level - 1, envs=Envs}, LookingFor)
    end.

get_var(Key, #gdm_state{curEnv=0, envs=Envs}) -> 
    % Current environment is 0, the global scope, should always succeed
    %io:format("Getting env at level ~p for key ~p~n",[0,Key]),
    E = maybe_keyfind_env(0, Envs),
    Map = E#gdm_env.vars,
    case maps:get(Key, Map, null) of
        null ->
            %io:format("Key ~p wasn't found, returning null~n", [Key]),
            null; %undefined, something has probably gone wrong
        A ->
            A
    end;
get_var(Key, St0 = #gdm_state{curEnv=Level, envs=Envs}) -> 
    %io:format("Getting env at level ~p for key ~p~n",[Level,Key]),
    E = maybe_keyfind_env(Level, Envs),
    Map = E#gdm_env.vars,
    case maps:get(Key, Map, null) of
        null ->
            % Recurse
            %io:format("Key ~p wasn't found, check ~p~n", [Key, Level-1]),
            get_var(Key, St0#gdm_state{curEnv=Level - 1});
        A ->
            A
    end.

% TODO: Simplify this, we don't need the complicated logic for evaluating the
% environment for a function because we only let them be defined at the global scope.
% Unless we decide to support classes. ?
put_fun(Key, Val, Where, St0) ->
    % Assume the environment is setup and get the current Env list
    Envs0 = St0#gdm_state.envs,
    %io:format("Current envs are: ~p~n", [Envs0]),
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
                        E = #gdm_env{id=Where}
                end,
                M0 = E#gdm_env.functions,
                % New map has the value we want inserted
                % Try to evaluate the expression as much as possible
                M1 = maps:put(Key, Val, M0),
                % Reinsert the map into the environment
                E1 = E#gdm_env{functions=M1},
                [ E1 | Envs0 ];
            % Found an env
            E0 -> 
                M0 = E0#gdm_env.functions,
                % New map has the value we want inserted
                % Try to evaluate the expression as much as possible
                M1 = maps:put(Key, Val, M0),
                % Reinsert the map into the environment
                E1 = E0#gdm_env{functions=M1},
                % Reinsert the environment into the EnvList
                lists:keyreplace(Where, #gdm_env.id, Envs0, E1)
        end,
    % Update the state (phew!)
    St1 = St0#gdm_state{envs=Envs1},
    St1.

walk_fun(Key, #gdm_state{curEnv=0, envs=Envs}, LookingFor) -> 
    % Current environment is 0, the global scope, should always succeed
    E = lists:keyfind(0, #gdm_env.id, Envs),
    Map = E#gdm_env.functions,
    Result = maps:is_key(Key, Map),
    if
        LookingFor == Result ->
            {Result, 0};
        true ->
            false % can't find the thing we want
    end;
walk_fun(Key, St0 = #gdm_state{curEnv=Level, envs=Envs}, LookingFor) -> 
    E = case lists:keyfind(Level, #gdm_env.id, Envs) of
        false ->
            #gdm_env{id=Level};
        Tuple ->
            Tuple
    end,
    Map = E#gdm_env.functions,
    %NewAcc = [ maps:is_key(Key, Map) | Acc ],
    Result = maps:is_key(Key, Map),
    if
        LookingFor == Result ->
            {Result, Level};
        true ->
            walk_fun(Key, St0#gdm_state{curEnv=Level - 1, envs=Envs}, LookingFor)
    end.

get_fun(Key, St0 = #gdm_state{curEnv=0, envs=Envs}) ->
    E = maybe_keyfind_env(0, Envs),
    Map = E#gdm_env.functions,
    case maps:get(Key, Map, null) of
        null ->
            null;
        A ->
            A
    end;
get_fun(Key, St0 = #gdm_state{curEnv=Level, envs=Envs}) ->
    E = maybe_keyfind_env(0, Envs),
    Map = E#gdm_env.functions,
    case maps:get(Key, Map, null) of
        null ->
            % Recurse
            get_fun(Key, St0#gdm_state{curEnv=Level-1});
        A ->
            A
    end.


maybe_keyfind_env(Level, Envs) ->
   case lists:keyfind(Level, #gdm_env.id, Envs) of
        false ->
            #gdm_env{};
        Tuple ->
            Tuple
   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit Tests                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if_test() ->
    T = gdminus_test:parse_file("examples/if.gdm"),
    walk(T).

arith_test() ->
    T = gdminus_test:parse_file("examples/arith.gdm"),
    walk(T).

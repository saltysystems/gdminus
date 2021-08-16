-module(gdminus_int).
% GDMinus Interpreter
% -------------------
% This module is rather dirty in the sense that it abuses the Erlang process
% dictionary to process the syntax tree of GDMinus rather than threading the
% state through every function.

-export([file/1]).

-record(state, {curEnv=0, curLoop=0, envs=maps:new(), breakers=maps:new()}).
-record(env, {functions=maps:new(), variables=maps:new()}).

% Open a file, walk the tree, nuke the process key in the process dict at the end.
file(Path) ->
    F = gdminus_test:parse_file(Path),
    erlang:put(state, #state{}),
    walk(F),
    % Nuke the state
    erlang:erase(state).

% Walk the tree, evaluating statements and expressions as it goes.
walk([]) ->
    ok;
walk([{Op, _Val1, _Val2} | Rest]) when 
      Op == '+'; Op == '-'; Op == '*'; Op == '/';
      Op == '=='; Op == '>='; Op == '<='; Op == '!';
      Op == '>'; Op == '<' ->
    % Nothing useful from evaluating these nodes since they're just exprs without
    % side-effects
    walk(Rest);
walk([{var, {name, _L, Name}} | Rest]) ->
    declare(Name, var),
    walk(Rest);
walk([{var, {name, _L, Name}, Val} | Rest]) ->
    declare(Name, expr(Val), var),
    walk(Rest);
walk([{'=', {name, _L, Name}, Val} | Rest]) ->
    assign(Name, expr(Val)),
    walk(Rest);
walk([{Op, Expression, Block} | Rest]) when Op == 'if'; Op == 'elif' ->
    %TODO: Prevent 'elif' if not preceeded by 'if'
    %TODO: Should we case this, or push the Rest into the statement function? Same for while, for..
    case ifelse(expr(Expression), Block) of
        ok -> walk(Rest);
        X -> 
            X % If there's a return statement nestled in there, we need to get the result out
    end;
walk([{Op, Block} | Rest]) when Op == 'else' ->
    case ifelse(else, Block) of
        ok -> walk(Rest);
        X -> 
            X % If there's a return statement nestled in there, we need to get the result out
    end;
walk([{while, Condition, Block} | Rest]) ->
    Expr = expr(Condition),
    incrementLoop(),
    while(Expr, Condition, Block),
    walk(Rest);
walk([{for, {name, _Line, Name}, Iter, Block} | Rest]) ->
    Expr = expr(Iter),
    incrementLoop(),
    for(Name, Expr, Block, Rest);
walk([{Op} | _Rest]) when Op == 'break'; Op =='continue' ->
    St0 = erlang:get(state),
    CurLoop = St0#state.curLoop,
    case CurLoop >= 1 of
        true ->
            Breakers = St0#state.breakers,
            St1 = St0#state{breakers=maps:put(CurLoop, Op, Breakers)},
            erlang:put(state, St1)
    end;
walk([{func, {name, _Line, Name}, Args, Block} | Rest]) ->
    define(Name, Args, Block),
    walk(Rest);
walk([{func_call, Name, Args} | Rest]) ->
    expr({func_call, Name, Args}),
    walk(Rest);
walk([{return, Expression} | _Rest]) ->
    expr(Expression);
walk([{match, Expression, Conditions} | Rest]) ->
    match(Expression, Conditions),
    walk(Rest);
walk([{return} | _Rest ]) ->
    null.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expressions                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expr({string, _Line, String}) ->
    String;
expr({number, _L, Value}) ->
    Value;
expr({name, _L, Variable}) ->
    get_variable(Variable);
expr({func_call, {name, _Line1, Name1}, {name, _Line2, Name2}, Args}) ->
    % Will return a value or null if the function is only called for side
    % effects.
    function(Name1 ++ "." ++ Name2, Args);
expr({func_call, {name, _Line, Name}, Args}) ->
    % Will return a value or null if the function is only called for side
    % effects.
    function(Name, Args);
expr({'+', Val1, Val2}) -> 
    overload_add(expr(Val1), expr(Val2));
expr({'-', Val1, Val2}) -> 
    expr(Val1) - expr(Val2);
expr({'/', Val1, Val2}) -> 
    expr(Val1) / expr(Val2);
expr({'*', Val1, Val2}) -> 
    expr(Val1) * expr(Val2);
expr({'==', Val1, Val2}) -> 
    expr(Val1) == expr(Val2);
expr({'!=', Val1, Val2}) -> 
    expr(Val1) /= expr(Val2);
expr({'>=', Val1, Val2}) -> 
    expr(Val1) >= expr(Val2);
expr({'<=', Val1, Val2}) -> 
    expr(Val1) =< expr(Val2);
expr({'>', Val1, Val2}) -> 
    expr(Val1) > expr(Val2);
expr({'<', Val1, Val2}) -> 
    expr(Val1) < expr(Val2);
expr({negation, Val}) ->
    negate(expr(Val));
expr(Other) ->
    Other.

negate(Val) when is_number(Val) ->
    0 - Val.

function(Name, Args) ->
    case builtin_function(Name, Args) of
        undefined -> 
            % Not a built-in, continue from the local function table
            local_function(Name, Args);
        Value ->
            Value
    end.

local_function(Name, Args) ->
    St0 = erlang:get(state),
    Environment = St0#state.curEnv,
    % Functions are only defined in the local scope (til we have Class support?)
    Return = case get_obj(func, Name, Environment) of
        {_, false} -> 
            throw("Locally scoped function undefined and built-in not found");
        {_, {Params, Block}} ->
            local_function_block(Params, Block, Args)
    end,
    Return.

local_function_block(Params, Block, Args) when length(Params) == length(Args) ->
    St0 = erlang:get(state),
    Environment = St0#state.curEnv,
    erlang:put(state, St0#state{curEnv=Environment + 1}),
    % Setup the local variables in the environment, called for its side effects
    setup_function_env(lists:zip(Args, Params)),
    Ret = walk(Block),
    % Remove the temporary environment and decrement the current state
    removeEnv(Environment+1),
    St1 = erlang:get(state),
    erlang:put(state, St1#state{curEnv=Environment}),
    Ret;
local_function_block(_Params, _Block, _Args) ->
    throw("Could not evaluate function: Wrong number of arguments").

setup_function_env([]) ->
    ok;
setup_function_env([{Arg, Param}  | Rest]) ->
    {name, _, Name} = Param,
    declare(Name, expr(Arg), func),
    setup_function_env(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Statements                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Declare the variable in the current environment. Behaviour depends on the
% caller - if called from a variable declaration then it checks for shadowing
% and errors if the same variable has already been declared in this scope.
% Otherwise if the caller is a function constructor then we just allow
% shadowing.
declare(Name, Caller) ->
    declare(Name, null, Caller).
declare(Name, Val, var) ->
    State = erlang:get(state),
    Env = State#state.curEnv,
    case get_obj(var, Name, Env) of
        {_, false} -> 
            put_obj(var, Name, Val, Env);
        X ->
            io:format("Got ~p~n", [X]),
            throw("Variable already exists in the current scope")
    end;
declare(Name, Val, func) ->
    State = erlang:get(state),
    Env = State#state.curEnv,
    put_obj(var, Name, Val, Env).

% Assign a value to a variable that exists in the current environment.
assign(Name, Val) ->
    State = erlang:get(state),
    Env = State#state.curEnv,
    case get_obj(var, Name, Env) of
        {_Env, false} ->
            throw("Variable not declared in the current scope");
        {Where, _CurrentValue} ->
            % Overwrite the current value
            put_obj(var, Name, Val, Where)
    end.

% If 'if' or 'elif' evaluate to false, just return. Otherwise if it's true, or
% the statement is an 'else', process the indented block of statements and
% expressions.
ifelse(false, _Block) ->
    ok;
ifelse(Op, Block) when Op == 'true'; Op == 'else' ->
    % Increase the environment level, walk the block, decrease.
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    erlang:put(state, St0#state{curEnv=Env + 1}),
    Ret = walk(Block),
    % Remove the temporary environment and decrement the current state
    removeEnv(Env+1),
    St1 = erlang:get(state),
    erlang:put(state, St1#state{curEnv=Env}),
    Ret.

% Define a new function
define(Name, Params, Block) ->
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    define(Name, Params, Block, Env).
define(_Name, _Params, _Block, Env) when Env > 0 -> 
    throw("Functions cannot be declared in this scope");
define(Name, Params, Block, Env) ->
    case get_obj(func, Name, Env) of
        {Env, false} ->
            put_obj(func, Name, {Params, Block}, Env);
        _ ->
            throw("Function is already defined")
    end.

% For loops
for(Name, Iter, Block, Rest) when is_integer(Iter) ->
    Expanded = [ {number, null, N} || N <- lists:seq(1,Iter) ],
    for(Name, Expanded, Block, Rest);
for(_Name, [], _Block, Rest) ->
    % Loop is finishe, decrement counter
    decrementLoop(),
    % Walk the rest of the tree
    walk(Rest);
for(Name, [Head|Tail] = Iter, Block, Rest) when is_list(Iter) ->
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    % Create a new environment for the loop to execute in
    erlang:put(state, St0#state{curEnv=Env + 1}),
    % Grab the iterator and declare it within an environment
    setup_function_env([{Head, {name, null, Name}}]),
    % false -> No breaks, so proceed as normal. 
    %                  1. Walk the block
    %                  2. Remove the env
    %                  3. Decrement current Env
    %                  4. Proceed to next loop
    % 'continue' -> Jump to the next iteration in the loop and walk the code
    %               block. i.e.,
    %                   1. Walk the block
    %                   2. Clean up the loop breaker
    %                   3. Remove the env
    %                   4. Decrement current Env
    %                   5. Proceed to next loop
    % 'break' -> Stop the loop, and clean up. i.e.,
    %                   1. Clean up the loop breaker
    %                   2. Remove the env
    %                   3. Decrement the current env
    %                   4. Call the last iteration of the loop
    case maybe_break() of
        false -> 
            walk(Block),
            removeEnv(Env+1),
            decrementEnv(),
            for(Name, Tail, Block, Rest);
        continue ->
            walk(Block),
            clearBreaker(),
            removeEnv(Env+1),
            decrementEnv(),
            for(Name, Tail, Block, Rest);
        break -> 
            clearBreaker(),
            removeEnv(Env+1),
            decrementEnv(),
            for(Name, [], Block, Rest)
    end.

% While loops
% Condition is not true, so continue walking the chain
while(false, _Condition, _Block) ->
    decrementLoop(),
    ok;
% Condition is currently true, so walk the Block and check for any breakers
while(true, Condition, Block) ->
    incrementEnv(),
    Env = getEnv(),
    case maybe_break() of
        false -> 
            walk(Block),
            removeEnv(Env),
            decrementEnv(),
            while(expr(Condition), Condition, Block);
        continue ->
            walk(Block),
            clearBreaker(),
            removeEnv(Env),
            decrementEnv(),
            while(expr(Condition), Condition, Block);
        break -> 
            clearBreaker(),
            removeEnv(Env),
            decrementEnv(),
            while(false, Condition, Block)
    end.

maybe_break() -> 
    St0 = erlang:get(state),
    % Get the current loop and breaker for this loop, if it exists.
    Loop = St0#state.curLoop,
    Breakers = St0#state.breakers,
    % if no breaker exists, just return false
    maps:get(Loop, Breakers, false).

% Match statements
% Iterate through the list of match conditions, stopping when a match is
% successful.
match(_Expression, []) ->
    ok;
match(_Expression, [{match_cond, {name, _L, "_"}, Block}]) ->
    % Head is the last condition in the list and can match any expression
    walk(Block);
match(Expression, [{match_cond, Condition, Block} |Tail]) ->
    case match(expr(Expression), expr(Condition), Block) of
        true ->
            ok;
        false ->
            match(Expression, Tail)
    end.

% If the condition is a success, then walk the block and return true so the
% match does not continue evaluating.
match(Val, Condition, Block) when Condition == Val ->
    walk(Block),
    true;
match(Val, Condition, _Block) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Funs for working with the state tree                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Find an object of a given type in the current scope, recursively traversing
% upwards through the tree as necessary
get_obj(Type, Name) ->
    State = erlang:get(state),
    Env = State#state.curEnv,
    get_obj(Type, Name, Env).
get_obj(Type, Name, 0) ->
    State = erlang:get(state),
    Envs = State#state.envs,
    EnvMap = maps:get(0, Envs, #env{}),
    Object = 
        case Type of
            func ->
                F = EnvMap#env.functions,
                maps:get(Name, F, false);
            var ->
                V = EnvMap#env.variables,
                maps:get(Name, V, false)
        end,
    {0, Object};
get_obj(Type, Name, EnvID) when EnvID > 0 -> 
    State = erlang:get(state),
    Envs = State#state.envs,
    EnvMap = maps:get(EnvID, Envs, #env{}),
    case Type of
        func ->
            F = EnvMap#env.functions,
            case maps:get(Name, F, false) of
                false -> 
                    get_obj(Type, Name, EnvID - 1);
                Object -> 
                    {EnvID, Object}
            end;
        var ->
            V = EnvMap#env.variables,
            case maps:get(Name, V, false) of
                false -> 
                    get_obj(Type, Name, EnvID - 1);
                Object -> 
                    {EnvID, Object}
            end
    end.

% Puts an object into the state table. Does not check scope rules, previous
% declarations, etc.
put_obj(Type, Name, Env) -> 
    put_obj(Type, Name, null, Env).
put_obj(Type, Name, Val, Env) -> 
    State = erlang:get(state),
    Envs0 = State#state.envs,
    EnvID = Env,
    % Return or create map for Env given the EnvID
    EnvMap0 = maps:get(EnvID, Envs0, #env{}),
    EnvMap1 = 
        case Type of
            func ->
                F0 = EnvMap0#env.functions,
                % Put the new function into the functions map
                F1 = maps:put(Name, Val, F0),
                EnvMap0#env{functions=F1};
            var ->
                V0 = EnvMap0#env.variables,
                % Put the new variable into the variables map
                V1 = maps:put(Name, Val, V0),
                % Put the variables map into the Envs1
                EnvMap0#env{variables=V1}
         end,
     % Put the new/updated environment into the Envs map
     Envs1 = maps:put(EnvID, EnvMap1, Envs0),
     % Update the state with the new environment map and return
     State1 = State#state{envs=Envs1},
     erlang:put(state, State1),
     ok.

incrementLoop() ->
    St0 = erlang:get(state),
    Loop = St0#state.curLoop,
    St1 = St0#state{curLoop=Loop+1},
    erlang:put(state, St1).

decrementLoop() ->
    St0 = erlang:get(state),
    Loop = St0#state.curLoop,
    if
        Loop > 0 ->
            St1 = St0#state{curLoop=Loop-1},
            erlang:put(state, St1);
        true ->
            ok
    end.

getLoop() ->
    St0 = erlang:get(state),
    St0#state.curLoop.

removeEnv(Env) ->
    St0 = erlang:get(state),
    Envs0 = St0#state.envs,
    St1 = St0#state{envs=maps:remove(Env, Envs0)},
    erlang:put(state, St1).

incrementEnv() ->
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    St1 = St0#state{curEnv=Env+1},
    erlang:put(state, St1).

decrementEnv() ->
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    if
        Env > 0 ->
            St1 = St0#state{curEnv=Env-1},
            erlang:put(state, St1);
        true ->
            ok
    end.

getEnv() ->
    St0 = erlang:get(state),
    St0#state.curEnv.

printState() ->
    St0 = erlang:get(state),
    io:format("Current state is: ~p~n", [St0]).

clearBreaker() ->
    St0 = erlang:get(state),
    Br0 = St0#state.breakers,
    Loop = St0#state.curLoop,
    Br1 = maps:remove(Loop, Br0),
    erlang:put(state, St0#state{breakers=Br1}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_variable({name, _L, Var}) ->
    get_variable(Var);
get_variable(Var) ->
    {_, Val} = get_obj(var, Var),
    Val.

% Provide two ways of using the '+' operator: adding numbers and concatinating
% strings.   
overload_add(Val1, Val2) when is_number(Val1), is_number(Val2) ->
    Val1 + Val2;
overload_add(Val1, Val2) when is_list(Val1), is_list(Val2) ->
    Val1 ++ Val2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Built-in functions callable from GDMinus                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
builtin_function("print", []) ->
    null;
builtin_function("print", [Head | Rest]) ->
    io:format("~p~n", [expr(Head)]),
    builtin_function("print", Rest);
builtin_function("OS.get_ticks_msec", []) ->
    erlang:system_time(millisecond);
builtin_function("OS.get_ticks_usec", []) ->
    erlang:system_time(microsecond);
builtin_function("str", [Arg]) ->
    str(expr(Arg));
builtin_function("abs", [Arg]) ->
    abs(expr(Arg));
builtin_function("acos", [Arg]) ->
    math:acos(expr(Arg));
builtin_function("asin", [Arg]) ->
    math:asin(expr(Arg));
builtin_function("atan", [Arg]) ->
    math:atan(expr(Arg));
builtin_function("atan2", [X,Y]) ->
    math:atan2(expr(X),expr(Y));
builtin_function("ceil", [Arg]) ->
    math:ceil(expr(Arg));
builtin_function("cos", [Arg]) ->
    math:cos(expr(Arg));
builtin_function("cosh", [Arg]) ->
    math:cosh(expr(Arg));
builtin_function("exp", [Arg]) ->
    math:exp(expr(Arg));
builtin_function("floor", [Arg]) ->
    math:floor(expr(Arg));
builtin_function("fmod", [X,Y]) ->
    math:fmod(expr(X),expr(Y));
builtin_function("log", [Arg]) ->
    math:log(expr(Arg));
builtin_function("max", [X,Y]) ->
    erlang:max(expr(X),expr(Y));
builtin_function("min", [X,Y]) ->
    erlang:min(expr(X),expr(Y));
builtin_function("pow", [X,Y]) ->
    math:pow(expr(X),expr(Y));
builtin_function("randf", []) ->
    rand:uniform();
builtin_function("randi", []) ->
    rand:uniform(trunc(math:pow(2,32) - 1));
builtin_function("randomize", []) ->
    rand:seed(exs1024s),
    null;
%builtin_function("range", [Arg]) ->
%    lists:seq(0, expr(Arg) - 1);
%builtin_function("range", [Arg1, Arg2]) ->
%    lists:seq(expr(Arg1), expr(Arg2));
builtin_function("round", [Arg]) ->
    erlang:round(expr(Arg));
builtin_function("sin", [Arg]) ->
    math:sin(expr(Arg));
builtin_function("sinh", [Arg]) ->
    math:sinh(expr(Arg));
builtin_function("sqrt", [Arg]) ->
    math:sqrt(expr(Arg));
builtin_function("tan", [Arg]) ->
    math:tan(expr(Arg));
builtin_function("tanh", [Arg]) ->
    math:tanh(expr(Arg));
builtin_function(_, _Args) ->
    % Not a built-in function
    undefined.

str(Arg) when is_integer(Arg) -> 
    integer_to_list(Arg);
str(Arg) when is_list(Arg) ->
    Arg;
str(Arg) when is_float(Arg) ->
    float_to_list(Arg).

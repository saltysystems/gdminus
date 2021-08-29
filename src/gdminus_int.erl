%%%--------------------------------------------------------------------------
%%% @copyright (C) 2021, Lincoln Bryant
%%% @doc GDMinus Interpreter.
%%%   This package facilitates interoperability between Godot and Erlang by
%%%   implementing a subset of the GDScript scripting language as an Erlang
%%%   module. A subset of the GDScript standard library has been made available
%%%   as well. Users may additionally define their own functions in Erlang and
%%%   make them available to GDMinus programs.  This source code is made
%%%   available under the Apache License v2.0
%%% @author Lincoln Bryant
%%% @end
%%%--------------------------------------------------------------------------

-module(gdminus_int).

% GDMinus Interpreter
% -------------------
% This module is rather dirty in the sense that it abuses the Erlang process
% dictionary to process the syntax tree of GDMinus rather than threading the
% state through every function.

-export([
    init/0,
    destroy/0,
    file/1, file/2,
    do/2, do/1,
    insert_function/2,
    insert_dict/2,
    scan_file/1,
    parse_file/1
]).

-record(state, {
    curEnv = 0,
    curLoop = 0,
    envs = maps:new(),
    breakers = maps:new(),
    appFuns = maps:new(),
    console = []
}).

-record(console, {
    stdout = [],
    stderr = []
}).

-record(env, {functions = maps:new(), variables = maps:new()}).

%%============================================================================
%% API functions
%%============================================================================

-spec init() -> ok.
%%----------------------------------------------------------------------------
%% @doc Initialize the state of the interpreter in the process dictionary
%% @end
%%----------------------------------------------------------------------------
init() ->
    erlang:put(state, #state{}),
    erlang:put(console, #console{}),
    ok.

-spec destroy() -> ok.
%%----------------------------------------------------------------------------
%% @doc Delete all stateful information in the process dictionary
%% @end
%%----------------------------------------------------------------------------
destroy() ->
    erlang:erase(state),
    erlang:erase(console),
    ok.

-spec insert_function(string(), fun()) -> ok.
%%----------------------------------------------------------------------------
%% @doc Insert an function into the GDScript function table. Functions may only
%%      take a list as an argument.
%% @end
%%----------------------------------------------------------------------------
insert_function(Name, Fun) ->
    St0 = erlang:get(state),
    AppFun0 = St0#state.appFuns,
    AppFun1 = maps:put(Name, Fun, AppFun0),
    logger:notice("Inserted new function ~p", [AppFun1]),
    St1 = St0#state{appFuns = AppFun1},
    erlang:put(state, St1),
    ok.

-spec insert_dict(string(), map()) -> ok.
%%----------------------------------------------------------------------------
%% @doc Inject an Erlang map to a GDScript dictionary at the global scope. The
%%      caller is responsible for ensuring types are GDScript-compatible, e.g.
%%      no BEAM atoms
%% @end
%%----------------------------------------------------------------------------
insert_dict(Name, Map) -> 
    global_declare(Name, Map, var).

-spec scan_file(list()) -> list().
%%----------------------------------------------------------------------------
%% @doc Read a file from disk and tokenize it to prepare for parsing.
%% @end
%%----------------------------------------------------------------------------
scan_file(Path) ->
    {ok, F} = file:read_file(Path),
    Fn = binary:bin_to_list(F),
    {ok, Tokens, _L} = gdminus_scan:string(Fn),
    % Fix up indents/dedents
    gdminus_scan:normalize(Tokens).

-spec parse_file(list()) -> list().
%%----------------------------------------------------------------------------
%% @doc Parse a list of tokens into GDScript grammar.
%% @end
%%----------------------------------------------------------------------------
parse_file(Path) ->
    Tokens = scan_file(Path),
    {ok, Tree} = gdminus_parse:parse(Tokens),
    Tree.

% Return the standard 3-tuple, plus a map with requested variables.
-spec do(list(), map()) -> {list(), list(), map(), tuple()}.
%%----------------------------------------------------------------------------
%% @doc Walk the parse tree, evaluating statements and expressions along the
%%      way. Output from print functions etc returned as lists representing
%%      Standard Out and Standard Error. A map will be returned with the final
%%      state of requested variables. Finally the state record itself will be
%%      returned.
%% @end
%%----------------------------------------------------------------------------
do(Stmt, Return) ->
    {ok, Tokens, _L} = gdminus_scan:string(Stmt),
    % fix up the indents and dedents
    NormalForm = gdminus_scan:normalize(Tokens),
    {ok, Tree} = gdminus_parse:parse(NormalForm),
    walk(Tree),
    Stdout = console_get(stdout),
    Stderr = console_get(stderr),
    R = return(Return),
    {Stdout, Stderr, R, erlang:get(state)}.

-spec do(list()) -> {list(), list(), tuple()}.
%%----------------------------------------------------------------------------
%% @doc Walk the parse tree, evaluating statements and expressions along the
%%      way. Output from print functions etc returned as lists representing
%%      Standard Out and Standard Error. The state record itself will be
%%      returned as well.
%% @end
%%----------------------------------------------------------------------------
do(Stmt) ->
    {ok, Tokens, _L} = gdminus_scan:string(Stmt),
    % fix up the indents and dedents
    NormalForm = gdminus_scan:normalize(Tokens),
    {ok, Tree} = gdminus_parse:parse(NormalForm),
    walk(Tree),
    Stdout = console_get(stdout),
    Stderr = console_get(stderr),
    {Stdout, Stderr, erlang:get(state)}.

%%----------------------------------------------------------------------------
%% @doc Open a file, tokenize, parse, evaluate, and return the final output to
%%      the caller.
%% @end
%%----------------------------------------------------------------------------
-spec file(list()) -> {list(), list()}.
file(Path) ->
    file(Path, default).

%%----------------------------------------------------------------------------
%% @doc Open a file, tokenize, parse, evaluate, and return the final output to
%%      the caller. Optionally return the final state of the program.
%% @end
%%----------------------------------------------------------------------------
-spec file(list(), atom()) -> {list(), list()} | {list(), list(), tuple()}.
file(Path, default) ->
    init(),
    {ok, F} = file:read_file(Path),
    Fn = binary:bin_to_list(F),
    {StdOut, StdErr, _State} = do(Fn),
    destroy(),
    {StdOut, StdErr};
file(Path, debug) ->
    init(),
    {ok, F} = file:read_file(Path),
    Fn = binary:bin_to_list(F),
    {StdOut, StdErr, State} = do(Fn),
    destroy(),
    {StdOut, StdErr, State}.

%%============================================================================
%% Walking the parse tree
%%============================================================================

% Walk the tree, evaluating statements and expressions as it goes.
walk([]) ->
    ok;
walk([{Op, _Val1, _Val2} | Rest]) when
    Op == '+';
    Op == '-';
    Op == '*';
    Op == '/';
    Op == '==';
    Op == '>=';
    Op == '<=';
    Op == '!';
    Op == '>';
    Op == '<'
->
    % Nothing useful from evaluating these nodes since they are just exprs without
    % side-effects
    walk(Rest);
walk([{enum, List} | Rest]) ->
    enum(List),
    walk(Rest);
walk([{const, {name, _L1, Name}, {number, _L2, Val}} | Rest]) ->
    declare(Name, Val, var),
    walk(Rest);
walk([{var, {name, _L, Name}} | Rest]) ->
    declare(Name, var),
    walk(Rest);
walk([{var, {name, _L, Name}, {dict, KVItems}} | Rest]) ->
    declare(Name, dictionary(KVItems), var),
    walk(Rest);
walk([{var, {name, _L, Name}, Val} | Rest]) ->
    declare(Name, expr(Val), var),
    walk(Rest);
walk([{'=', {kv, Name, Key}, Val} | Rest]) ->
    {name, _L, N} = Name,
    kv_update(N, expr(Name), expr(Key), expr(Val)),
    walk(Rest);
walk([{'=', {name, _L, Name}, Val} | Rest]) ->
    assign(Name, expr(Val)),
    walk(Rest);
walk([{Op, Expression, Block} | Rest]) when Op == 'if'; Op == 'elif' ->
    %TODO: Prevent 'elif' if not preceeded by 'if'
    %TODO: Should we case this, or push the Rest into the statement function? Same for while, for..
    case ifelse(expr(Expression), Block) of
        ok ->
            walk(Rest);
        X ->
            % If there is a return statement nestled in there, we need to get the result out
            X
    end;
walk([{Op, Block} | Rest]) when Op == 'else' ->
    case ifelse(else, Block) of
        ok ->
            walk(Rest);
        X ->
            % If there is a return statement nestled in there, we need to get the result out
            X
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
walk([{Op} | _Rest]) when Op == 'break'; Op == 'continue' ->
    St0 = erlang:get(state),
    CurLoop = St0#state.curLoop,
    case CurLoop >= 1 of
        true ->
            Breakers = St0#state.breakers,
            St1 = St0#state{breakers = maps:put(CurLoop, Op, Breakers)},
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
walk([{return} | _Rest]) ->
    'Null'.

%%============================================================================
%% Expressions
%%============================================================================

expr({string, _Line, String}) ->
    String;
expr({number, _L, Value}) ->
    Value;
expr({name, _L, Variable}) ->
    get_variable(Variable);
%TODO : Clean this up - it's become very hacky. The parser can't tell the
%       difference between a class method invocation vs a dictionary acess.
expr(
    {func_call, {kv, {name, _Line1, Name1}, {string, _Line2, Name2}}, Args}
) ->
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
expr({dict, List}) ->
    % Declaration
    dictionary(List);
expr({kv, Name, Key}) ->
    keyvalue(expr(Name), expr(Key));
expr(List) when is_list(List) ->
    % Arrays
    eval(List);
expr(Other) ->
    % NOTE: This function allows Erlang terms to leak into GDscript if you aren't careful
    Other.

negate(Val) when is_number(Val) ->
    0 - Val.

% Check to see if a function is built in, otherwise check the function table.
function(Name, Args) ->
    case local_function(Name, Args) of
        undefined ->
            % Not a built-in, see if it's in the application function table
            E = eval(Args),
            case application_function(Name, E) of
                undefined ->
                    % Not there either, check if it's a built-in
                    builtin_function(Name, E);
                Value ->
                    Value
            end;
        Value ->
            Value
    end.

% Key value access can either be for an array or a dictionary
% which have slightly diff semantics
keyvalue(Name, Key) when is_map(Name) ->
    maps:get(Key, Name);
keyvalue(Name, Key) when is_list(Name) ->
    % it seems like Arrays should
    lists:nth(Key + 1, Name).

%%============================================================================
%% Statements
%%============================================================================

% Declare the variable in the current environment. Behaviour depends on the
% caller - if called from a variable declaration then it checks for shadowing
% and errors if the same variable has already been declared in this scope.
% Otherwise if the caller is a function constructor then we just allow
% shadowing.
declare(Name, Caller) ->
    declare(Name, 'Null', Caller).

declare(Name, Val, var) ->
    State = erlang:get(state),
    Env = State#state.curEnv,
    case get_obj(var, Name, Env) of
        {_, false} ->
            put_obj(var, Name, Val, Env);
        _X ->
            throw("Variable already exists in the current scope")
    end;
declare(Name, Val, func) ->
    State = erlang:get(state),
    Env = State#state.curEnv,
    put_obj(var, Name, Val, Env).

% Special version of declare that always happens at the global scope
global_declare(Name, Val, var) ->
    Env = 0,
    case get_obj(var, Name, Env) of
        {_, false} ->
            put_obj(var, Name, Val, Env);
        _X ->
            throw("Variable already exists in the current scope")
    end.

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

dictionary([]) ->
    maps:new();
dictionary(List) ->
    dictionary(List, maps:new()).

dictionary([], Map) ->
    Map;
dictionary([{kv, {_Type1, _Line1, Key}, Val} | T], Map1) ->
    Map2 = maps:put(Key, expr(Val), Map1),
    dictionary(T, Map2).

% Can update either Dicts or Arrays
kv_update(Name, Map, Key, Val) when is_map(Map) ->
    % Updating a dictionary
    Map1 = maps:put(Key, Val, Map),
    assign(Name, Map1);
kv_update(Name, List, Key, Val) when is_list(List) ->
    % TODO: Lists may not be the right way to represent arrays
    % internally, since updating an element in the middle of a
    % list isn't that great. 3 operations on the list itself to
    % generate List1 seems like it would be a performance sap.
    Index = lists:nth(Key + 1, List),
    List1 =
        lists:sublist(List, Index - 1) ++
            [Val] ++
            lists:nthtail(Index, List),
    assign(Name, List1).

% If 'if' or 'elif' evaluate to false, just return. Otherwise if it is true, or
% the statement is an 'else', process the indented block of statements and
% expressions.
ifelse(false, _Block) ->
    ok;
ifelse(Op, Block) when Op == 'true'; Op == 'else' ->
    % Increase the environment level, walk the block, decrease.
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    erlang:put(state, St0#state{curEnv = Env + 1}),
    Ret = walk(Block),
    % Remove the temporary environment and decrement the current state
    removeEnv(Env + 1),
    St1 = erlang:get(state),
    erlang:put(state, St1#state{curEnv = Env}),
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
    Expanded = [{number, 'Null', N} || N <- lists:seq(1, Iter)],
    for(Name, Expanded, Block, Rest);
for(_Name, [], _Block, Rest) ->
    % Loop is finishe, decrement counter
    decrementLoop(),
    % Walk the rest of the tree
    walk(Rest);
for(Name, [Head | Tail] = Iter, Block, Rest) when is_list(Iter) ->
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    % Create a new environment for the loop to execute in
    erlang:put(state, St0#state{curEnv = Env + 1}),
    % Grab the iterator and declare it within an environment
    setup_function_env([{Head, {name, 'Null', Name}}]),
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
            removeEnv(Env + 1),
            decrementEnv(),
            for(Name, Tail, Block, Rest);
        continue ->
            walk(Block),
            clearBreaker(),
            removeEnv(Env + 1),
            decrementEnv(),
            for(Name, Tail, Block, Rest);
        break ->
            clearBreaker(),
            removeEnv(Env + 1),
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

% Match statements
% Iterate through the list of match conditions, stopping when a match is
% successful.
match(_Expression, []) ->
    ok;
match(_Expression, [{match_cond, {name, _L, "_"}, Block}]) ->
    % Head is the last condition in the list and can match any expression
    walk(Block);
match(Expression, [{match_cond, Condition, Block} | Tail]) ->
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
match(_Val, _Condition, _Block) ->
    false.

% Enums are lists of constants with sequentially increasing value. Iterate through the list, assigning values
enum(List) ->
    enum(List, 0).

enum([], _N) ->
    ok;
enum([{name, _L, Name} | Tail], N) ->
    declare(Name, N, var),
    enum(Tail, N + 1).

%%============================================================================
%% Funs for working with the state tree
%%============================================================================

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
% UNUSED
%put_obj(Type, Name, Env) ->
%    put_obj(Type, Name, 'Null', Env).

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
                EnvMap0#env{functions = F1};
            var ->
                V0 = EnvMap0#env.variables,
                % Put the new variable into the variables map
                V1 = maps:put(Name, Val, V0),
                % Put the variables map into the Envs1
                EnvMap0#env{variables = V1}
        end,
    % Put the new/updated environment into the Envs map
    Envs1 = maps:put(EnvID, EnvMap1, Envs0),
    % Update the state with the new environment map and return
    State1 = State#state{envs = Envs1},
    erlang:put(state, State1),
    ok.

incrementLoop() ->
    St0 = erlang:get(state),
    Loop = St0#state.curLoop,
    St1 = St0#state{curLoop = Loop + 1},
    erlang:put(state, St1).

decrementLoop() ->
    St0 = erlang:get(state),
    Loop = St0#state.curLoop,
    if
        Loop > 0 ->
            St1 = St0#state{curLoop = Loop - 1},
            erlang:put(state, St1);
        true ->
            ok
    end.

%UNUSED TODO
%getLoop() ->
%    St0 = erlang:get(state),
%    St0#state.curLoop.

removeEnv(Env) ->
    St0 = erlang:get(state),
    Envs0 = St0#state.envs,
    St1 = St0#state{envs = maps:remove(Env, Envs0)},
    erlang:put(state, St1).

incrementEnv() ->
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    St1 = St0#state{curEnv = Env + 1},
    erlang:put(state, St1).

decrementEnv() ->
    St0 = erlang:get(state),
    Env = St0#state.curEnv,
    if
        Env > 0 ->
            St1 = St0#state{curEnv = Env - 1},
            erlang:put(state, St1);
        true ->
            ok
    end.

getEnv() ->
    St0 = erlang:get(state),
    St0#state.curEnv.

% TODO unused
%printState() ->
%    St0 = erlang:get(state),
%    io:format("Current state is: ~p~n", [St0]).

clearBreaker() ->
    St0 = erlang:get(state),
    Br0 = St0#state.breakers,
    Loop = St0#state.curLoop,
    Br1 = maps:remove(Loop, Br0),
    erlang:put(state, St0#state{breakers = Br1}).

% Append any "print" statements or otherwise to the Console
console_append(Val) ->
    console_append(Val, stdout).

console_append(Val, Channel) when Channel == 'stdout' ->
    St0 = erlang:get(console),
    Console0 = St0#console.stdout,
    St1 = St0#console{stdout = [Val | Console0]},
    erlang:put(console, St1);
console_append(Val, Channel) when Channel == 'stderr' ->
    St0 = erlang:get(console),
    Console0 = St0#console.stderr,
    St1 = St0#console{stderr = [Val | Console0]},
    erlang:put(console, St1).

console_get(Channel) ->
    St0 = erlang:get(console),
    case Channel of
        stdout ->
            lists:reverse(St0#console.stdout);
        stderr ->
            lists:reverse(St0#console.stderr)
    end.

%% Print any messages
%console_print() ->
%    console_print(stdout).
%
%console_print(Opt) when Opt == 'stdout' ->
%    St0 = erlang:get(console),
%    Console = St0#console.stdout,
%    console_print(lists:reverse(Console));
%console_print(Opt) when Opt == 'stderr' ->
%    St0 = erlang:get(console),
%    Console = St0#console.stderr,
%    console_print(lists:reverse(Console));
%console_print([]) ->
%    ok;
%console_print([H|T]) ->
%    io:format("~p~n", [H]),
%    console_print(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
return(List) ->
    return(List, maps:new()).

return([], Acc) ->
    Acc;
return([Key | Rest], Acc0) ->
    % Get the requested variable out of the State
    Value = get_variable(Key),
    Acc1 = maps:put(Key, Value, Acc0),
    return(Rest, Acc1).

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

maybe_break() ->
    St0 = erlang:get(state),
    % Get the current loop and breaker for this loop, if it exists.
    Loop = St0#state.curLoop,
    Breakers = St0#state.breakers,
    % if no breaker exists, just return false
    maps:get(Loop, Breakers, false).

application_function(Name, Args) ->
    St0 = erlang:get(state),
    AppFuns = St0#state.appFuns,
    case maps:get(Name, AppFuns, undefined) of
        undefined ->
            undefined;
        Fun ->
            Fun(Args)
    end.

local_function(Name, Args) ->
    St0 = erlang:get(state),
    Environment = St0#state.curEnv,
    % Functions are only defined in the local scope (til we have Class support?)
    Return =
        case get_obj(func, Name, Environment) of
            {_, false} ->
                undefined;
            {_, {Params, Block}} ->
                local_function_block(Params, Block, Args)
        end,
    Return.

local_function_block(Params, Block, Args) when length(Params) == length(Args) ->
    St0 = erlang:get(state),
    Environment = St0#state.curEnv,
    erlang:put(state, St0#state{curEnv = Environment + 1}),
    % Setup the local variables in the environment, called for its side effects
    setup_function_env(lists:zip(Args, Params)),
    Ret = walk(Block),
    % Remove the temporary environment and decrement the current state
    removeEnv(Environment + 1),
    St1 = erlang:get(state),
    erlang:put(state, St1#state{curEnv = Environment}),
    Ret;
local_function_block(_Params, _Block, _Args) ->
    throw("Could not evaluate function: Wrong number of arguments").

setup_function_env([]) ->
    ok;
setup_function_env([{Arg, Param} | Rest]) ->
    {name, _, Name} = Param,
    declare(Name, expr(Arg), func),
    setup_function_env(Rest).

eval(List) when is_list(List) ->
    eval(List, []);
eval(Expr) ->
    expr(Expr).

eval([], Acc) ->
    lists:reverse(Acc);
eval([H | T], Acc) ->
    eval(T, [expr(H) | Acc]).

%%===========================================================================
%% Built-in functions callable from GDMinus
%%===========================================================================
builtin_function("print", []) ->
    'Null';
builtin_function("print", [Head | Rest]) when is_map(Head) ->
    % Closer to GDScript native formatting, but not quite. TODO replace ','
    % with ':'
    console_append(list_to_tuple(maps:to_list(Head))),
    builtin_function("print", Rest);
builtin_function("print", [Head | Rest]) ->
    console_append(Head),
    builtin_function("print", Rest);
builtin_function("OS.get_ticks_msec", []) ->
    erlang:system_time(millisecond);
builtin_function("OS.get_ticks_usec", []) ->
    erlang:system_time(microsecond);
builtin_function("str", [Arg]) ->
    str(Arg);
builtin_function("abs", [Arg]) ->
    abs(Arg);
builtin_function("acos", [Arg]) ->
    math:acos(Arg);
builtin_function("asin", [Arg]) ->
    math:asin(Arg);
builtin_function("atan", [Arg]) ->
    math:atan(Arg);
builtin_function("atan2", [X, Y]) ->
    math:atan2(X, Y);
builtin_function("ceil", [Arg]) ->
    math:ceil(Arg);
builtin_function("cos", [Arg]) ->
    math:cos(Arg);
builtin_function("cosh", [Arg]) ->
    math:cosh(Arg);
builtin_function("exp", [Arg]) ->
    math:exp(Arg);
builtin_function("floor", [Arg]) ->
    math:floor(Arg);
builtin_function("fmod", [X, Y]) ->
    math:fmod(X, Y);
builtin_function("log", [Arg]) ->
    math:log(Arg);
builtin_function("max", [X, Y]) ->
    erlang:max(X, Y);
builtin_function("min", [X, Y]) ->
    erlang:min(X, Y);
builtin_function("pow", [X, Y]) ->
    math:pow(X, Y);
builtin_function("randf", []) ->
    rand:uniform();
builtin_function("randi", []) ->
    rand:uniform(trunc(math:pow(2, 32) - 1));
builtin_function("randomize", []) ->
    rand:seed(exs1024s),
    'Null';
builtin_function("range", [Arg]) ->
    lists:seq(0, Arg - 1);
builtin_function("range", [Arg1, Arg2]) ->
    lists:seq(Arg1, Arg2);
builtin_function("round", [Arg]) ->
    erlang:round(Arg);
builtin_function("sin", [Arg]) ->
    math:sin(Arg);
builtin_function("sinh", [Arg]) ->
    math:sinh(Arg);
builtin_function("sqrt", [Arg]) ->
    math:sqrt(Arg);
builtin_function("tan", [Arg]) ->
    math:tan(Arg);
builtin_function("tanh", [Arg]) ->
    math:tanh(Arg);
builtin_function(_, _Args) ->
    % Not a built-in function, and presumably the local function call also failed.
    throw("Function not defined or not implemented").

str(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
str(Arg) when is_list(Arg) ->
    Arg;
str(Arg) when is_float(Arg) ->
    float_to_list(Arg).

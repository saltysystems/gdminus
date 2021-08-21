gdminus
=====

gdminus is an implementation of GDScript for Erlang, allowing Erlang
applications to share code with [Godot](https://godotengine.org/). 

Scope and Plans
-----
The intent is to implement a strict subset of GDScript that facilitates
communication between Erlang-based game servers and Godot-based clients. 

gdminus will probably not implement keywords supporting coroutines, signals, or
networking RPC. These include `yield`, `signal`, `remote`, `master`, `puppet`,
and so on.

gdminus does not currently support classes, but may in the future if they are too onerous to implement.

Current State
-----
A working lexer, parser and tree-walking interpreter have been developed that
support a generous subset of the language including:
  * Arithmetic expressions and Boolean comparison operators.
  * If/Else statements
  * While/For statements with `break` and `continue`
  * Match statements
  * Functions, including a number of built-in functions. See e.g. `examples/fib.gd` or `examples/math.gd` 
  * Dictionaries and arrays

The lexer has been implemented using [leex](https://github.com/rvirding/leex)
and the parser is implemented via [yecc](https://erlang.org/doc/man/yecc.html).
The interpreter takes a great deal of inspiration from
[Luerl](https://github.com/rvirding/luerl) as well as [Crafting
Interpreters](https://craftinginterpreters.com/).

Performance
-----
gdminus is easily 10x slower than Godot for many applications. In fact, gdminus
will probably never be as fast as Godot's built-in GDScript virtual machine.
This is due to inherent slowness in using a treewalking interpreter, overheads
incurred by implementing a procedural and mutable language in the BEAM, and
general programmer inefficiency :)

The first 25 numbers in the fibonacci sequence using the recursive
implementation, for example:

Implementation             | Speed 
-------------------------- | ----- 
Godot                      | 88ms  
gdminus (OTP/24 with JIT)  | 673ms 
gdminus (OTP/22)           | 713ms 

(on an i7-7600U with absolutely no performance optimizations)


Known Caveats
-----
There have been no efforts to date to ensure gdminus rigorously follows
GDScript semantics, nor have any of the re-implementations of various built-in
functions been verified for correctness. Caution is advised around floating
point math especially.

The implementation is decidedly uncouth for Erlang code for any number of
reasons. The author is rather unhappy with abusing the process dictionary to
hold state in the lexer and interpreter.

Build / Test 
-----
```
    $ rebar3 shell
    1> gdminus_test:regenerate().
    2> c(gdminus_scan).
    3> c(gdminus_parse).
    4> gdminus_int:file("examples/minimal.gdm")
```
    
Examples
-----
Calculate the first 25 [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number):
```
func fib(n):
    if n < 2:
        return 1
    else:
        return fib(n-1) + fib(n-2)

func time():
    return OS.get_ticks_msec()

var start = time()
print(fib(25))
var end = time()
print("Time: " + str(end - start) + "ms")
```

gdminus will return a 3-tuple to the shell in the format
`{Stdout,Stderr,FinalState}`. Standard out and standard error are represented
as lists with each new line representing a list item. The final state contains
the user-defined function table plus any variables defined and so on.
```
1> gdminus_int:file("examples/fib.gdm").
{[121393,"Time: 1395ms"],
 [],
 {state,0,0,
  #{0 =>
     {env,
      #{"fib" =>
         {[{name,1,"n"}],
          [{'if',
            {'<',{name,2,"n"},{number,2,2}},
            [{return,{number,3,1}}]},
           {else,
            [{return,
              {'+',
               {func_call,{name,5,"fib"},[{'-',{name,...},{...}}]},
               {func_call,{name,5,"fib"},[{'-',{...},...}]}}}]}]},
        "time" =>
         {[],
          [{return,
            {func_call,
             {{name,8,"OS"},{string,8,"get_ticks_msec"}},
             []}}]}},
      #{"end" => 1629431927532,"start" => 1629431926137}}},
  #{},[]}}
```

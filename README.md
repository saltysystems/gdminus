gdminus
=====

gdminus is a partial GDScript implementation for Erlang, allowing Erlang applications to share code with [Godot](https://godotengine.org/). 

Scope and Plans
-----
The intent is to implement a strict subset of GDScript that facilitates communication between Erlang-based game servers and Godot-based clients. 

gdminus will probably not implement keywords supporting coroutines, signals, or networking RPC. These include `yield`, `signal`, `remote`, `master`, `puppet`, and so on.

gdminus will probably support classes in some form if they're not too onerous to implement.

Current State
-----
A working lexer, parser and tree-walking interpreter have been developed that support a generous subset of the language including:
  * Arithmetic expressions and Boolean operators
  * If/Else statements
  * While/For statements with `break` and `continue`
  * Match statements
  * Functions
  * A number of built-in functions. See e.g. `examples/fib.gd` or `examples/math.gd` 

The lexer has been implemented using [leex](https://github.com/rvirding/leex) and the parser is implemented via [yecc](https://erlang.org/doc/man/yecc.html). The interpreter takes a great deal of inspiration from [Luerl](https://github.com/rvirding/luerl) as well as [Crafting Interpreters](https://craftinginterpreters.com/).

Performance
-----
gdminus will probably never be as fast as Godot's built-in GDScript virtual machine. This is due to inherent slowness in using a treewalking interpreter, overheads incurred by the BEAM virtual machine, and general programmer inefficiency :)

In rudimentary benchmarks with OTP 24 (JIT-enabled), gdminus is easily 10x slower than Godot for many applications. 

Known Caveats
-----
There have been no efforts to date to ensure gdminus rigorously follows GDScript semantics, nor have any of the re-implementations of various built-in functions been verified for correctness. Caution is advised around floating point math especially.

The implementation is decidedly uncouth for Erlang code for any number of reasons. The author is rather unhappy with abusing the process dictionary to hold state in the lexer and interpreter.

Embarassingly, the lexer only supports space-indented code blocks right now. 

Build / Test 
-----
    $ rebar3 shell
    1> gdminus_test:regenerate().
    2> c(gdminus_scan).
    3> c(gdminus_parse).
    4> gdminus_int:file("examples/minimal.gdm")
    
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

gdminus will return the output to the shell plus the final state of the code: 
```
7> gdminus_int:file("examples/fib.gdm").
121393
"Time: 2936ms"
{state,0,0,
       #{0 =>
             {env,#{"fib" =>
                        {[{name,1,"n"}],
                         [{'if',{'<',{name,2,"n"},{number,2,2}},
                                [{return,{number,3,1}}]},
                          {else,[{return,{'+',{func_call,{name,5,"fib"},
                                                         [{'-',{name,5,"n"},{number,5,1}}]},
                                              {func_call,{name,5,"fib"},
                                                         [{'-',{name,5,[...]},{number,5,...}}]}}}]}]},
                    "time" =>
                        {[],
                         [{return,{func_call,{name,8,"OS"},
                                             {name,8,"get_ticks_msec"},
                                             []}}]}},
                  #{"end" => 1629091915893,"start" => 1629091912957}}},
       #{}}
```

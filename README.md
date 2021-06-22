gdminus
=====

gdminus is a partial GDScript implementation for Erlang, allowing Erlang applications to share code with [Godot](https://godotengine.org/). 

Scope
-----
The intent is to implement a strict subset of GDScript that facilitates communication between Erlang-based game servers and Godot-based clients. 

gdminus will probably not implement keywords supporting coroutines, signals, or networking RPC. These include `yield`, `signal`, `remote`, `master`, `puppet`, and so on.

Current State
-----
A lexer has been implemented using [leex](https://github.com/rvirding/leex) and a parser is being implemented slowly via [yecc](https://erlang.org/doc/man/yecc.html). 

Build
-----
    $ rebar3 compile

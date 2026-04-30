# Placo

A very primitive, simplistic programming language. This is a learning project.
It's named after the [Placozoa](https://en.wikipedia.org/wiki/Placozoa),
which are among, if not actually, the simplest animals on Earth.
This was basically a project to

Placo currently supports integers and functions only, in case that gives you
an idea of its production-readiness.

## Implementation Overview

Placo is a tree-walking interpreter.
The AST is built with simple Scheme structs.
I wrote a generalized visitor that can be used both to evaluate or print
a parse tree.
The visitor callbacks for the evaluator return functions that take an environment
and return the evaluated value.
The printer callbacks accept an output-stream and nesting depth.
Side-effects are freely sprinkled about.

The parser is hand-written recursive descent with a bit of precedence climbing.
A recursive-descent parser is enormously satisfying to write.
If you haven't, you should really try it.
Scheme makes it easy to write higher-order parsers like `repeated`.

The scoping ends up a bit funny.
The bodies of functions are evaluated in the environment in
which the function was defined...
specifically the environment *before* the function was defined.
To write a recursive function, you need to explicitly pass it to itself as a continuation.

The most obvious next feature is string types.
I may also try implementing explicitly continuation-based operations,
or see if my generic visitor framework can be used for things like
partial evaluation.


# Placo

A very primitive, simplistic programming language. This is a learning project.
It's named after the [Placozoa](https://en.wikipedia.org/wiki/Placozoa),
which are among, if not actually, the simplest animals on Earth.

This was basically a project to refresh myself on programming language fundamentals
while learning Racket.
Placo currently supports integers and functions only, in case that gives you
an idea of its production-readiness.

Here's a sample program, also available as `sample/factorial.placo`.

```
# nice linear-time factorial
def fac-impl(i acc k)
    if i = 1 then
        acc
    else
        k(i - 1 acc * i k)
    end
end

def factorial(i)
    fac-impl(i 1 fac-impl)
end

def main()
    print(factorial(5))
end
```

Top-level definitions are evaluated in order.
There's also a top-level `let` (see `sample/foo.placo`).
Also, isn't it funny how you don't actually need commas to separate expressions in an argument list?

## Implementation Overview

Place is implemented in Racket, because I thought it would be fun and educational.
This turned out to be correct.

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

## Samples and Usage

You can run a placo file by piping it into the standard input of the placo script. If you're in the project directory and have racket installed:

```
racket placo.rkt < sample/fizzbuzz.placo
```

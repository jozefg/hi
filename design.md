# Design Notes

Before I actually dive into writing code I figured it'd be a good idea
to actually write down what I plan to do.

## What Language Are We Interpreting?

Well we're surely not going to try for all of GHC's Haskell. In fact,
I don't even want to try for Haskell 98. Instead, we'll try to
interpret Haskell 98 - modules + FFI. This will greatly simplify type
checking an interpretation since we get to see all the code at
once. It does preclude actually using this for "real world stuff", but
I think that's an acceptable loss.

Since there will be no modules, prelude is also out. I'll eventually
have to compile a list of all the primitives I'll provide but I think
I'll start with

 - `->`
 - `()` (both the type and value)
 - `(,)` (both the type and value)
 - `Char`
 - `Int`
 - `Double`
 - `intPlus`
 - `intMinus`
 - `doublePlus`
 - `doubleMinus`
 - `Bool`
 - `True`
 - `False`
 - `[a]`
 - `[]`
 - `(:)`
 - `IO`
 - `ioReturn`
 - `ioBind`
 - `putChar`
 - `getChar`

That ought to be enough for now.

At the end of this project we should be able to write

``` haskell
    type String = List Char

    putStrLn :: String -> IO ()
    putStrLn [] = return ()
    putStrLn (c : rest) = putChar c `ioBind` (\_ -> putStrLn rest)

    main :: IO ()
    main = putStrLn "abc"
```

## How Is The Interpreter Organized

The interpreter is split into five main parts. Each of these will
completely separate and make no assumptions outside of the
prerequisites listed.

### Parsing
For now this will be done by `haskell-src-exts`. This is mostly
because I have no faith or time to muddle about with a parser. For now
we'll just take one off the shelf.

### Syntactic Validation

Since we're only interpreting a small subset of Haskell we're going to
have to prune whatever AST we're given. In this portion of the
interpreter we'll traverse the code and fail if we encounter an
unknown construct.

I still want to keep the (too big) AST from src-exts because

 1. Source location information
 2. Off the shelf pretty-printing

Both of which we'll need for producing errors during type
checking. Therefore assume nothing of the input, but ensure the output
only contains our subset of Haskell.

### Typing

This portion still works across the src-exts AST, but assumes that it
is pared down to the language we understand.

Based on Mark Jones's paper, we'll type check this AST and produce a
type annotated AST that is significantly simpler than
src-ext's. Further, this AST will have no source information since
from here on out we won't be producing an errors except for runtime
exceptions.

This will likely be the fiddliest portion of the interpreter.

### Simplification

Haskell contains a lot of stuff we don't want to think about when
we're interpreting. In order to deal with this we'll make a number of
passes on the AST to remove constructs we don't want. This operates on
the type annotated AST so requires the code it handles to be well typed.

 - Function syntax -> simple declarations + lambdas
 - `where` -> `let`
 - `if` -> `case`
 - type classes -> records + functions
 - Annotate closures on bindings + lambdas
 - Lift lambdas into local let statements
 - Other stuffs?

Once all of these simplifications have been applied, we'll output a
new type of AST which is still type annotated but much much simpler
than the input one.

### Interpretation

Finally the AST we're left with is basically the source language for
the STG machine. We'll just simulate this machine and hey presto we've
done it!

## Other Squishy Things
### Documentation

In order to live up to the "you" part of the project's name I really
ought to document this process a bit. I'll do my best to write my
thoughts down in little markdown documents like this one.

I really hope to transform these into a meaningful series of blog post
like entries, but we'll see if it actually gets to that.

### What Should the Reader Know

For anyone curious of following the process. I'll document things as
much as I can but I'll still make a number of fundamental assumptions

 1. You know Haskell enough to have written a project or two on your
    own (have you tried Write Yourself a Scheme?)
 2. You know what a closure, thunk, free variable, and type checker is
 3. You have gone through Write Yourself a Scheme or something similar

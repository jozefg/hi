# Design Notes

Before I actually dive into writing code I figured it'd be a good idea
to actually write down what I plan to do.

## What Language Are We Interpreting?

Well we're surely not going to try for all of GHC's Haskell. In fact,
I don't even want to try for Haskell 98. Instead, we'll try to
interpret Haskell 98 - modules. This will greatly simplify type
checking an interpretation since we get to see all the code at
once. It does preclude actually using this for "real world stuff", but
I think that's an acceptable loss.

Since there will be no modules, prelude is also out. I'll eventually
have to compile a list of all the primitives I'll provide but I think
I'll start with

 - `->`
 - `()` (both the type and value)
 - `Char`
 - `int2char`, `char2int`
 - `Int`, `Double`
 - `intPlus`, `intMinus`, ...
 - `doublePlus`, `doubleMinus`, ...
 - `Bool`, `True`, and `False`
 - `[]`, `:`, `[a]`
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

### Typing

### Simplification

### Interpretation

## Other Squishy Things

### Documentation
### What Should the Reader Know

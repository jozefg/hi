## Hi - A Haskell interpreter

There aren't nearly enough toy Haskell interpreters. Here's my attempt
at one. Some of the design is fleshed out in the notes directory.

This interpreter only handles a single file, it doesn't attempt to
handle modules or imports or anything like that. Aside from this
though it is a reasonably complete implementation of core Haskell 98.

The interpreter follows roughly the pipeline

    Parsing (haskell-src-exts)
                ⇓
        Pruning the AST
                ⇓
     Type Checking and similar
                ⇓
       Strip Syntactic Sugar
                ⇓
        Closure Conversion
                ⇓
         Lambda Lifting
                ⇓
    STG Machine (PUSH-ENTER style)

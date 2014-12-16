## Notes on the Type Checker

The type checker is likely going to be one of the more complicated
pieces of Hi. In part, this is because it is solely responsible for
looking at a confusing looking AST and checking to make sure it's well
formed.

In order to do this it needs to do a few things

 - Make sure each signature has a corresponding body
 - Resolve all the names referenced in `infix` declarations
 - Check to make sure type synonyms form a DAG
 - Then actually type check (which has a lot of nontrivial steps)

To make this more feasible, I'm going to split each of these phases
into their own modules. They'll each traverse the AST separately and
check their respective properties.

One important deviation from Jones' paper is our handling of kinds. We
can't assume everything is annotated with its kind because it
isn't. Furthermore, inferring what kinds go where is actually a pretty
subtle process since different type declarations can affect the arity
of others. Haskell's story here is actually quite weird.

Haskell 98 says that you ought to just infer kinds for one particular
`data` declaration and whatever you can't work out should be defaulted
to `*`. GHC 6.4 thought this was too strict and opted to infer kinds
for the whole module and *then* default everything.

I'm going to opt for GHC 6.4's approach for a bit of fun. The idea is
that type inference will be quite similar to type inference. We'll set
up all the different type expressions found in `data` and `class`
declarations and just do basic constraint solving across the lot of
them. From here whatever isn't inferred will be defaulted to `*`. This
is a bit trickier but really not that much harder and I think the
results are worth it.

To give a concrete example, Haskell 98 would reject this but we'll
accept it.

``` haskell
    data A f = A (f (B f))
    data B a = B
```

Our constraint solver will see

 - `f (B f) ~ *`
 - `B ~ X -> *`

And correctly infer that `f :: X` and therefore `X = * -> *`.

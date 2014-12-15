## Notes on the Type Checker

The type checker is likely going to be one of the more complicated
pieces of Hi. In part, this is because it is solely responsible for
looking at a confusing looking AST and checking to make sure it's well
formed.

In order to do this it needs to do a few things

 - Resolve all the names referenced in `infix` declarations
 - Make sure each signature has a corresponding body
 - Check to make sure type synonyms form a DAG
 - Then actually type check (which has a lot of nontrivial steps)

To make this more feasible, I'm going to split each of these phases
into their own modules. They'll each traverse the AST separately and
check their respective properties.

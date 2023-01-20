---
title: Implementing environment-based evaluation of recursive functions in OCaml
---

We are told in undergraduate courses on programming languages that function
application performs a substitution. Indeed, most theoretical accounts of the
semantics of programming languages express function application in terms of a
substitution. This operation even has a fancy math-name: beta-reduction.

However, a realistic implementation cannot possibly implement function
application in this way. It's simply too slow to be traversing the syntax tree
to eliminate variables via substitution. Instead, the approach most commonly
used is to track a map from variables to values called an _environment_.
Rather than eagerly substitute away in the body `e` the bound variable `x` of an
abstraction such as `fun x -> e` at the moment of an application such as
`(fun x -> e) v`, the interpreter instead augments the current environment with
the mapping `x -> v`. When the variable `x` is encountered during the evaluation
of the body `e`, the interpreter looks up `x` in the mapping and finds `v`. The
outcome is the same as if we had eagerly computed the substitution `[v/x]e` and
evaluated the result, but we were able to eliminate a traversal of the syntax
tree for `e`!

(There is of course a connection between (eager) substitutions and environments:
the substitution and evaluation operations become fused when working in an
environment-based evaluator. In other words, an environment is a lazy
substitution.)

This environment-based approach to evaluation raises some eyebrows.
The expression being evaluated might contain variables now -- that's the
whole point! The upshot is that the implementation of first-class functions
needs some additional care when using an environment. To see the problem,
consider this definition of a function `g : int -> int -> int`.

```ocaml
let g = fun x ->
  let y = x * 2 in
  fun z -> y + z
```

Now suppose further that we want to evaluate `g 2`.

In a substitution-based approach, we replace `x` with `2`, calculate `2 * 2`,
replace `y` with `4`, and ultimately produce the function `fun z -> 4 + z`. No
problem: the variables in the function body are just `z`, the parameter of the
function.

In an environment-based approach, we associate `x` with `2`, calculate `x * 2`
by looking up `x`, associate `y` with `4`, and ultimately produce the function
`fun z -> y + z` as we don't evaluate under a `fun`.

Clearly, we cannot return _only_ `fun z -> y + z` at the end. That would be
throwing away the fact that `y -> 4`, which is essential to evaluating that
function! In other words, that function contains a _free variable_ `y`! What we
need to do is to return not just `fun z -> y + z`, but also the current
environment `y -> 4`. This way, if we later apply the resulting function, then
we restore the environment `y -> 4`, augment it with `x -> v` for whatever `v`
we're applying to, and successfully evaluate `y + z` since the environment has a
mapping for both `y` and `z`.

This is the key idea: functions evaluate to a pair consisting of the function
code together with the current environment. Such a pair is called a _closure_.



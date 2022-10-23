---
title: SAT solving with higher-order continuations
---

Finding satisfying assignments to the variables of a boolean formula, called
SAT-solving, [is a model of a whole class of problems][SAT-NPC]. This means that
there are many problems out there whose solutions can be implemented in terms of
finding such a satisfying assignment.

I want to share a particularly mind-bending and (I think) elegant way of finding
a satisfying assignment to a boolean formula by brute-force search. The idea is
to do the search using [continuation-passing style (CPS)][CPS], but with one
trick.

## Continuations

At every point of a program's execution, it is associated to some _execution
state._ This state represents what remains to be done with the result of
evaluating the current expression. Normally, this execution state is implicit,
and in particular, is implied by the programming language's order of operations.

For example, suppose we are performing a postorder traversal of a binary tree,
collapsing it by summing the integer values contained within each node of the
tree.

```ocaml
type tree = Leaf | Node of tree * int * tree
let rec sum = function
  | Leaf -> 0
  | Node (l, x, r) ->
    let a = sum l in
    let b = sum r in
    a + b + x (* writing the order of operations explicitly *)
```

When we encounter some node of the tree, we had to get there somehow. On one
hand, if this node is the root of the tree, then there is nothing to do with the
calculated sum; this is the starting position. The calculated sum is simply the
result of the program. On the other hand, if we reached this node as a right
subtree of a parent node, then what remains to be done with the result of this
node is to add it with the parent node's left subtree sum (`a`) and integer
value (`x`).

This order of operations is captured concrete _call stack._

Continuations enter the picture as a _representation of the call stack._

Rather than let the language handle the call stack, we will represent it
ourselves using functions, in particular using closures.

```ocaml
let rec sum' t k = match t with
  | Leaf -> k 0
  | Node (l, x, r) ->
    sum l @@ fun a ->
    sum r @@ fun b ->
    k (a + b + x)
```

In this implementation, rather than using `let` and relying on the language
implementation to create stack frames for us, we are passing explicit functions
expressing what to do with the result.

The parameter `k` is a _continuation._ It contains an accumulation of pending
operations to be performed once the result of processing the given tree is
ready. We can make a few observations about this code.

* Each recursive call is now a tail-call! Due to [tail-call optimization][TCO],
  no new stack frames will be allocated by the compiler at all. We are doing it
  all by hand.
* The continuations themselves form a stack. By looking at the
  [free variables][FV] of the continuations we construct when making recursive
  calls, we see that they refer to the outer continuation `k`. This effectively
  forms a linked list of continuations -- there's the stack!
* Specifically, the continuation of `sum l` has the free variables `r`, `k`, and `x`;
  the continuation of `sum r` has `a`, `k`, and `x`.
  The compiler will allocate heap objects to store these captured variables --
  these are the stack frames!
  
Overall, this merely looks like a more cumbersome way of writing code.
However, it does buy us some interesting abilities. Since the execution state is
explicit, it becomes possible to store it! This enables us to "time travel" back
to a previous point of the program's execution. It is exactly this ability that
I exploit in implementing a backtracking SAT-solver.

## SAT-solving with continuations

A pedestrian way of implementing a backtracking SAT solver is first to write an
evaluator for boolean formulas, and then to write an enumerator of truth
assignments. We try each truth assignment with the evaluator until we find one
that makes the formula true.

Here is what such an evaluator looks like.

```ocaml
type formula =
  | Conj of formula * formula
  | Disj of formula * formula
  | Neg of formula
  | Var of string
  
type env = (string * bool) list

let rec eval (r : env) = function
  | Var x -> List.assoc x r
  | Neg e -> not (eval r e)
  | Conj (e1, e2) -> eval r e1 && eval r e2
  | Disj (e1, e2) -> eval r e1 || eval r e2
```

What remains to implement a solver in this way is to collect all the variable
names present in the formula, and enumerate all possible assignments.

Instead, we can do it all in one pass using CPS. 

The crucial observation is that when we encounter a variable `x`, we may or may
not have already picked a value for it. If we haven't picked a value for `x` yet,
then we want to try to _evaluate the rest of the tree_ with `x = true` and if
that doesn't work, then with `x = false`.

What if that still doesn't work? Then we have to go back to the previous
variable and try a different value for it.

What if we run out of previous variables to go back to? Then the formula is
unsatisfiable; we will have tried every truth assignment and not found one that
makes the formula true.

The mind-bending aspect of this solution is that there are _two stacks_, not
just one. We will have an "ordinary" call stack for the evaluator, but also have
a secondary stack whose frames are introduced when we pick tentative values for
variables. This secondary stack's frames will contain a copy of the evaluator
stack at that point, enabling the solver to jump back to that point and try
_evaluating the rest of the tree_ but with an adjusted truth assignment, in
particular with `x = false`.

I twice emphasize "evaluate the rest of tree" above because that is precisely
the current continuation of the evaluator at the point that we encounter the
variable.

Let's see how the code looks.

```ocaml
let rec solve (r : env) (fail : unit -> 'r) (phi : formula)
    (assign : env -> bool -> (unit -> 'r) -> 'r) : 'r =
  match phi with
  | Var x -> begin match List.assoc_opt x r with
    | Some b -> (* we already have a value for `x` *)
      assign r b fail
    | None -> (* we don't have a value for `x` *)
      assign ((x, true) :: r) true @@ fun () ->
      assign ((x, false) :: r) false fail
    end
  | Conj (e1, e2) -> 
    solve r fail e1 @@ fun r b1 fail ->
    if b1 then (* short-circuiting *)
      solve r fail e2 @@ fun r b2 fail -> assign r (b1 && b2) fail
    else
      assign r false fail
  | Disj (e1, e2) ->
    solve r fail e1 @@ fun r b1 fail ->
    if b1 then
      assign r true fail
    else
      solve r fail e2 @@ fun r b2 fail ->
      assign r (b1 || b2) fail
```

When I first wrote this code, I was frankly surprised that it worked, because I
didn't really understand why it worked. Since then I've thought a lot about it,
and I will try to offer an intuitive explanation.

Let's understand the `fail` continuation as a stack of _variables and
execution states_ to return to. To see this, consider that the only place where
we construct a value for this continuation is in the case `Var`, and the free
variables of that continuation are `assign`, `x`, `r`, and `fail`.

* By capturing `assign`, the new failure continuation obtains a copy of the
  evaluator call stack at this point.
* We must capture `x` and `r` to form the adjusted assignment.
* Capturing the outer `fail` forms a linked list of these frames consisting of
  evaluator stacks and variables.
  
Given that `assign` is the main continuation, we should understand its inputs as
what are supposed to be the outputs of `solve`.

* The `env` input is a (possibly) updated version of `r` with more
  variable bindings accumulated.
* The `bool` input is the boolean value of `phi` under the returned assignment.
* The `unit -> 'r` input is a (possibly) updated version of `fail` with more
  stack frames added to it, i.e. more `Var` nodes to jump back to to try
  alternatives.
  
There is a little something missing from `solve` above. How exactly are we
supposed to call it? In other words, what initial continuations should we
provide, to recover a function `formula -> env option` that decides whether a
formula is satisfiable, giving in that case a satisfying assignment?

We need to pick an initial frame for the `fail` stack. This frame will be
reached only if we try all the values of all the variables -- we return `None` here
as the formula is unsatisfiable.

We need to pick an initial frame for the `assign` stack. This frame will be
reached when we finish evaluating the formula, and we will be given the
assignment that resulted in the given `bool`. Also, we will receive the failure
continuation, which we can call to try different values of the variables until
we get a `bool` we like. Of course, the initial `env` that we pass will be
empty, as we haven't seen any variables yet.

```ocaml
let solve' (phi : formula) : env option =
  solve [] (fun () -> None) phi @@ fun r result fail ->
  if result then Some r else fail ()
```
  
### Unexpected benefits

In contrast with the so-called pedestrian solver that enumerates assignments and
tries them, the implementation of `solve` above has an interesting performance
benefit: it caches partial results. Shout-out to [Max
Kopinsky][MK] ([github][JKTKops]) for noticing this.
To be clear, it isn't obvious that this is a net benefit, since we are using
extra memory to hold an evaluator stack for each variable, but it is interesting
nonetheless!

To see this caching, suppose we have a fairly large formula with eight
variables.
Let's order the variables according to when they first appear in a
left-to-right traversal. This is exactly the order in which variables are added
to the environment by `solve`.
Under this order, by the time we encounter the eighth variable, we have already
evaluated some chunk of the formula that didn't depend on that variable. When we
finish evaluating the formula and see that `result = false`, we call `fail`,
jumping back to the eighth variable, and we reevaluate with `x8 = false` _only
the part of the formula that depended on that variable!_

## Conclusion

This article explains execution states, continuations as representations
thereof, and shows an interesting use of them to implement a backtracking search
by storing continuations in the closures of other continuations.
This layering of continuations gives rise to a fascinating kind of control flow,
enabling backtracking to certain _leaves_ of a tree to make different choices at
those points.

It is not immediately obvious how one can recover a nice, "direct"
implementation of `solve` above, without using continuations.
There is, however, a general way of eliminating higher-order functions called
[defunctionalization][DEFUN]. In short, whereas CPS gives us control of stack
frames, defunctionalization forces us moreover to manually construct and
destruct closures. Defunctionalizing `solve` gives us a program using
explicit stacks in the form of lists of frames, represented as explicit data
structures. This representation makes it plainly obvious that the failure
continuation stores a copy of the evaluator continuation.

Looking at the defunctionalization of `solve` in more details would be the topic
of a future post!

[CPS]: https://en.wikipedia.org/wiki/Continuation-passing_style
[SAT-NPC]: https://en.wikipedia.org/wiki/NP-completeness 
[TCO]: https://en.wikipedia.org/wiki/Tail_call
[FV]: https://en.wikipedia.org/wiki/Free_variables_and_bound_variables
[JKTKops]: https://github.com/jktkops
[MK]: https://www.maxkopinsky.com/

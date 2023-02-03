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
Rather than eagerly substitute away the variable `x` in the body `e` of an
abstraction such as `fun x -> e` at the moment of an application such as `(fun x
-> e) v`, the interpreter instead augments the current environment with the
mapping `x -> v`. When the variable `x` is encountered during the evaluation of
the body `e`, the interpreter looks up `x` in the mapping and finds `v`. The
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
we restore the environment `y -> 4`, augment it with `z -> v` for whatever `v`
we're applying to, and successfully evaluate `y + z` since the environment
contains a mapping for both `y` and `z`.

This is the key idea: functions evaluate to a *pair* consisting of the function
code *together with the current environment*. Such a pair is called a _closure_.

The code for an evaluator using this strategy is beautifully simple.

```ocaml
type name = string

type value =
  | Clo of env * name * exp

and env = (name * value) list

and exp =
  | Fun of name * exp
  | App of exp * exp
  | Var of name
  
let rec eval env = function
  | Fun (x, e) -> Clo (env, x, e)
  | Var x -> List.assoc x env
  | App (e1, e2) -> match eval env e1 with
    | Clo (env', x, e) ->
      let v = eval env e2 in
      (* In evaluating the body of the closure,
         we restore its saved environment and
         augment it with a mapping for
         the variable `x`.*)
      eval ((x, v) :: env') e
```

Alas the beauty of this approach will slightly suffer if we allow functions to
be defined in terms of themselves.

## Introducing recursion

The problem with recursion is the problem with recursion. Let me be more
specific by first extending the syntax of the language to include a recursive
binding construct `rec`.

At a high level, it works like this: `rec f -> e` binds the name `f` to the
value that results from evaluating `e`, and moreover the variable `f` is in
scope in the expression `e`. For example, assuming we had in our language
`if`-expressions, integers, and arithmetic we could define a function that sums
the first `n` natural numbers: `rec sum -> fun n -> if n = 0 then 0 else n + sum (n-1)`.

```ocaml
type value =
  | Clo of env * name * exp
  
and env = (name * value) list

and exp =
  | Fun of name * exp
  | Rec of name * exp
  | App of exp * exp
  | Var of name
```

And now let's try to implement the high-level description above in `eval`.

```ocaml
let rec eval env = function
  | Rec (f, e) ->
    (* We want to evaluate `e` with `f` in scope, i.e. present in `env`.
       However, the value to which we want to bind `f` is the value we
       get from evaluating `e`.
       In other words, to be able to evaluate `e` with `f` in scope,
       we need to be able to evaluate `e` with `f` in scope!
    *)
    let rec v = eval ((f, v) :: env) e in
    v
```

<img
  class="figure figure-natural"
  alt="Thanos meme: I used the stones to destroy the stones, but adapted for recursion."
  src="/images/thanos-recursion.jpg">
  
Perhaps that code hurts your head a little. That's fine. It hurts OCaml's head too!

To see why, recall that OCaml is a strict language: before it can make the
recursive call to `eval`, it evaluates the argument `((f, v) :: env)`. In
evaluating that, it would have to look up `v`, and thus enter an infinite loop.

(By the way, in Haskell, a lazy language, the approach above works flawlessly.)

OCaml, nauseous from staring into the recursive abyss we wrote, rejects our
code at compile-time with the annoying error message "this kind of expression is
not allowed on the right-hand side of `let rec`". And honestly, we can't really
blame OCaml.

Fortunately, we aren't doomed. The problem ultimately comes from the fact that
our language's recursive binding construct is more general than the recursive
binding construct `let rec` of OCaml. Therefore, we can't directly model our
language's recursion in terms of OCaml's.

What we will do to resolve this is adjust our definition of an environment.
Instead of mapping names to values, we will map them to mutable references to
optional values, i.e. `value option ref`. This way, when we evaluate the body
`e` of a `Rec (f, e)` expression, we associate `f` to `None`. Then, when we're
done evaluating `e` to some `v`, we'll _reassociate_ `f` to `Some v`.

This has a very important consequence. Whenever we look up a name in the
environment, we will need to inspect the option; if it should turn out to be
`None`, then what this means is that we're trying to eagerly use a recursive
value we're in the middle of defining, as in something like `rec x -> x + 1`.
To handle these situations, we will just crash the interpreter with an error
message like "infinite loop".
Effectively, we only allow an occurrence of a recursively defined name under a
`fun`, since these delay the evaluation of the function body until later, when
the function is called. This is pretty much the restriction that OCaml enforces
statically.

```ocaml
type value =
  | Clo of env * name * exp
  
and env = (name * value option ref) list

and exp =
  | Fun of name * exp
  | Rec of name * exp
  | App of exp * exp
  | Var of name
  
let rec lookup env x = match !(List.assoc x env) with
  | Some v -> v
  | None -> failwith "infinite recursion"

let rec eval env = function
  | Rec (f, e) ->
    let v_box = ref None in
    let v = eval ((f, v_box) :: env) e in
    v_box := Some v;
    v
  | Fun (x, e) -> Clo (env, x, e)
  | Var x -> lookup env x
  | App (e1, e2) -> match eval env e1 with
    | Clo (env', x, e) ->
      let v_box = ref (Some (eval env e2)) in
      eval ((x, v_box) :: env') e
```

With this implementation, definitions such as `rec x -> x + 1` produce a runtime
error without actually entering the infinite loop. On the other hand,
definitions such as `rec sum -> fun n -> if n = 0 then 0 else n + sum (n-1)` are
accepted as we expect.

## Conclusion

This article illustrates the difference between substitution-based and
environment-based evaluation, introduces the concept of a closure as a tuple of
a function and an environment, motivates the need for closures when implementing
first-class functions in an environment-based setting, and shows how extend the
implementation to handle recursion.

This approach to handling recursion crops up in various other settings. It
happens that we want to define a _value_ in terms of itself, but can't express
this directly in a strict language like OCaml. The "trick" is to use a mutable
reference as a placeholder, and to fill it in later.
This approach is called "tying the recursive knot".

There is a related approach called "untying the recursive knot", where we take a
ordinary recursive function and apply a separation the concerns: we isolate the
recursion itself from the operation done to the results of the recursive calls.
For further reading, I recommend
[this][1]
article about exactly this refactoring.

EDIT: I also published [an article][2] demonstrating this refactoring in JavaScript.

[1]: http://typeocaml.com/2015/01/25/memoize-rec-untying-the-recursive-knot/
[2]: /posts/2023-01-22-refactoring-asynchronous-recursion-continuations.html

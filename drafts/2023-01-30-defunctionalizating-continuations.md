---
title: Functional programmers hate this one trick
---

Originally developed as a technique for implementing compilers for
(higher-order) functional languages, _defunctionalization_ (d17n) is a program
transformation that eliminates higher-order functions.
It is based on the following observation: although there might be infinitely
many possible functions one might pass to a higher-order function, there are
only finitely many functions that are _actually_ passed in any given program.
Therefore, we can define a (finite) data type to represent our choice of
function and we can define an interpreter for this data type that recovers the
behaviour of the original function.

One situation we might want to employ d17n as programmers is in networked
applications --
higher-order functions give us lots of expressive power, but we unfortunately
can't send functions over the network!
In fact, you might have already done this without
realizing it. Rather than send a function, which as more or less impossible, we
send some representation of a function that the remote side interprets to
execute the function we wanted.
The canonical example in web development is the use of a `return_to` URL parameter:
when a user attemps to perform an action while not logged in, we redirect them
to a login page with a `return_to` URL parameter. Once the user logs in, they
are redirected to the URL stored in that parameter. That parameter is precisely
a defunctionalized _continuation_.

This article will demonstrate d17n first for the standard higher-order function
`filter`, followed by a discussion of defunctionalized continuations.

## Defunctionalizing `filter`

Let's see d17n in action with an example. Suppose our program filters lists.

```ocaml
let rec filter p = function
  | [] -> []
  | x :: xs -> if p x then x :: filter p xs else filter p xs

let _ = filter (fun x -> x mod 2 = 0) [1;2;3;4;5;6;7;8;9]
```

Now let's see the defunctionalized form of this program.

```ocaml
(* This is the data structure that represents our choice of function. *)
type predicate = IsEven

(* This is the interpreter for this data structure,
   which recovers the behaviour of the function. *)
let apply p_rep x = match p_rep with
  | IsEven -> x mod 2 = 0

(* This is the defunctionalized filter.
   Instead of receiving a function as input, it receives a
   value of type `predicate`, which is a _representation_ of a function. *)
let rec filter_df p_rep = function
  | [] -> []
  | x :: xs ->
    (* We pass the predicate to our interpreter. *)
    if apply p_rep x
    then x :: filter_df p_rep xs
    else filter_df p_rep xs

let _ = filter_df IsEven [1;2;3;4;5;6;7;8;9]
```

Look, no more higher-order functions!

That was a quite simple example though.
Let's see how to accommodate more complicated functions one by one.

First, what happens when the function we pass to `filter` is a _closure_, i.e.
it contains variables that are not its parameters?

```ocaml
let divisible_by k l = filter (fun x -> x mod k = 0) l
```

Notice that the function `fun x -> x mod k = 0` refers to `k`, which is not a
parameter of the function. To accommodate this, we will add a constructor to our
type `predicate` called `IsDivisible`, and that new constructor will crucially
have one field to store the value of this `k`. Then, when our interpreter
`apply` matches on the `predicate` it can recover the value of `k` and use it to
recover the behaviour of the original function.

```ocaml
type predicate = IsEven | IsDivisible of int

let apply p_rep x = match p_rep with
  | IsEven -> x mod 2 = 0
  | IsDivisible k -> x mod k = 0

(* The implementation of `filter` itself is unchanged. *)

let divisible_by k l = filter_df (IsDivisible k) l
```

Next, what happens when we combine multiple predicates into one? For example, we
might want to filter a list to select all elements that satisfy _two_
properties. Actually, we can define this as a separate combinator, which takes
two functions as input and produces a function as output.

```ocaml
let both f g = fun x -> f x && g x
```

Notice that the type of `f` and of `g` is also the type of `both f g`, namely
`'a -> bool`. That's precisely the higher-order type that we're eliminating via
d17n. This will end up making our representation type `predicate` into a recursive type.

```ocaml
type predicate =
  | IsEven
  | IsDivisible of int
  | Both of predicate * predicate

(* And correspondingly, our implementation of `apply` will be recursive too. *)
let rec apply p_rep x = match p_rep with
  | IsEven -> x mod 2 = 0
  | IsDivisible k -> x mod k = 0
  | Both (p1, p2) -> apply p1 x && apply p2 x
```

Now the astute reader might have noticed that our type `predicate` is actually
quite limiting: it only works for one type and in this case that's `int`.
The crux of the issue is that the predicate `IsDivisible` and `IsEven` only work
for `int`, whereas the predicate `Both (p1, p2)` should work for any `x : 'a`
provided that both `p1` and `p2` are predicates that work on `'a`.
More to the point, what if we had a predicate `IsPalidrome` that should work on on `string`?
How would we define a well-typed `apply`? It would need to be able to accept either a string or an
int, requiring that the given predicate be "for a string" or "for an int".

It is possible to further generalize the type `predicate` by making it into a
_generalized algebraic datatype_ (GADT). With a GADT, it becomes possible to implement a well-typed
`apply`, but a demonstration of this will need to wait for a future article.

## Defunctionalizing continuations

One amazing use of higher-order functions is a technique called
continuation-passing style (CPS). In this style of programming, rather than write
a function that returns its result normally, we instead write a function that
returns its result by passing it to another function. The upshot is that every call in a CPS
program is a tail-call. In the presence of [tail-call optimization][TCO], such programs do not use
the call stack as function calls are implemented (more or less) as simple `jump` instructions.
For the functional programmer, this means we have `goto` in OCaml!

A few months ago, I wrote [an article][sat-cps] explaining CPS in some detail
and showing a mindblowing use of it to implement a backtracking search through a
tree. I recommend reading that article before the rest of this one.

That article presents a simple definition of boolean formulas and an evaluator
for such formulas:

```ocaml
type formula =
  | Conj of formula * formula
  | Disj of formula * formula
  | Neg of formula
  | Var of string

type name = string
type env = (name * bool) list

let rec eval (r : env) = function
  | Var x -> List.assoc x r
  | Neg e -> not (eval r e)
  | Conj (e1, e2) -> eval r e1 && eval r e2
  | Disj (e1, e2) -> eval r e1 || eval r e2
```

The challenge is this: devise a way to find for a given `phi : formula` a
satisfying assignment to its variables, i.e. a value `r : env` such that `eval r
phi = true`, _in "one pass"._
Of course, it can't actually be in one pass because this is an NP-complete
problem; that's why I put quotes around "one pass". What I mean by "one pass" is
that we want to solve this formula without _separately_ enumerating all the
possible truth assignments of the variables and evaluating them.

I found a way to do this using CPS. The core idea of the below algorithm is that we want to save
the current execution state whenever we encounter a variable we haven't seen before. These occur in
the _leaves_ of the expression tree. If we find at the end of evaluating the tree that the result
is `false`, then we successively jump back to those saved states to try a different value for the
corresponding variable.

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
  | Neg e ->
    solve r fail e @@ fun r b fail -> assign r (not b) fail
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

(* From the CPS solver, we recover a function `formula -> env option` that decides
   whether the given formula is satisfiable, giving the satisfying assignment
   in that case.

   To do so we provide the initial continuations for `fail` and `assign`:
   - The initial `fail` continuation is reached if we have exhausted every
     assignment to the variables. The formula is therefore unsatisfiable.
   - The initial `assign` continuation is reached when we finish evaluating
     the whole expression, having worked out an assignment `r : env` under
     which the expression's value is `res : bool`.
*)
let solve_enter (phi : formula) : env option =
  solve [] (fun () -> None) phi (fun r res fail -> if res then Some r else fail ())
```

What's challenging and mysterious about this implementation is that the `assign` continuation,
which represents a normal return from `solve`, takes in addition to an `env` and a `bool` a
function of type `unit -> 'r`.
_This_ function represents an _abnormal_ return, like throwing an exception.
Therefore, _we can undo a return from `solve`_ by having the continuation we pass as
`assign` call its given failure continuation.

The most interesting and important part of the algorithm is in the `None`
subcase of the `Var` case. In that case, we have encountered a variable for
which we don't have an assigned value in the environment `r : env`.
So we call `assign` with an extended environment _and_ an augmented failure
continuation. It is exactly here that we express the idea "try to return `true` here
_but if that fails_ then return `false`."

At first it seems terrifying to try to defunctionalize this, but fortunately,
d17n is a completely systematic, mechanical process.

1. For every type of function that is passed, we define a new datatype.
   We define one interpreter for each of these types, although at this point all
   we can do is work out the signature of the interpreter, as we haven't decided what the
   constructors of these new types will be.
2. Find every position where we pass an anonymous function.
   Each of those functions becomes a constructor of the datatype corresponding
   to the function's type.
   Then, we have to identify for each anonymous function all the variables it
   contains that aren't its parameters -- the fancy math-name for those is _free
   variables_.
   The types of the free variables become the fields of the constructor corresponding to the
   function.
3. Find every place where we _call_ a function received through a parameter;
   these will become calls to the `apply` interpreter we will write.
   Replace every anonymous function with its corresponding constructor generated by the above
   process.
4. Implement apply to recover the original functions

We'll follow these steps one by one in modifying the implementation of `solve`.

### Step 1: type definitions

There are two higher-order types we seek to eliminate in the original program:
we define a type `failure` to represent the function `unit -> 'r` and a type
`assign` to represent the function `env -> bool -> (unit -> 'r) -> 'r`.

There is sadly a small wrinkle arising from the fact that without using a GADT, we can't express
polymorphism in our defunctionalized program. Therefore, we will have to decide what `'r` is
going to be. We observe that the initial continuations return `env option`, so that is
what we fix `'r` to be. The interpreters we define will be `apply_failure : failure -> env option`
and `apply_assign : assign -> env -> bool -> failure -> env option`.

```ocaml
type failure = ...
type assign = ...

let apply_failure (sf : failure) : env option = failwith "todo"
let apply_assign (sa : assign) (r : env) (b : bool) (sf : failure) : env option =
  failwith "todo"
```

### Step 2: constructor definitions

Here's the code for the CPS solver, this time annotated to identify all the anonymous functions
that are passed as arguments. Annotations `F-n` indicate an anonymous function passed as a `fail`
continuation whereas `A-n` indicate a function passed as an `assign` continuation.

```ocaml
let rec solve (r : env) (fail : unit -> 'r) (phi : formula)
    (assign : env -> bool -> (unit -> 'r) -> 'r) : 'r =
  match phi with
  | Var x -> begin match List.assoc_opt x r with
    | Some b -> assign r b fail
    | None ->
      assign ((x, true) :: r) true @@ fun () ->
      assign ((x, false) :: r) false fail (* F-1 *)
    end
  | Neg e ->
    solve r fail e @@ fun r b fail -> (* A-1 *)
    assign r (not b) fail
  | Conj (e1, e2) ->
    solve r fail e1 @@ fun r b1 fail -> (* A-2 *)
    if b1 then
      solve r fail e2 @@ fun r b2 fail -> (* A-3 *)
      assign r (b1 && b2) fail
    else
      assign r false fail
  | Disj (e1, e2) ->
    solve r fail e1 @@ fun r b1 fail -> (* A-4 *)
    if b1 then
      assign r true fail
    else
      solve r fail e2 @@ fun r b2 fail -> (* A-5 *)
      assign r (b1 || b2) fail

let solve_enter (phi : formula) : env option =
  solve []
    (fun () -> None) (* F-2 *)
    phi
    (fun r res fail -> if res then Some r else fail ()) (* A-6 *)
```

Since only two functions are passed as the `fail` continuation, let's start there. The initial
continuation has no free variables, so its constructor will just be `Failed` with no fields.
On the other hand, the function `F-1` has the free variables `assign`, `x : name`, `r : env`, and
`fail`.  This leads to the following definition.

```ocaml
type assign = ...
type failure =
  | Failed
  | Retry of assign * name * env * failure
```

And we can refactor this by observing that this pair of constructors give rise to a list structure:

```ocaml
type failure_frame = assign * name * env
type failure = failure_frame list
```

Next, we apply the same reasoning to the functions passed as `assign`. We can immediately observe
that a list structure will again arise as each function passed as `assign` refers to the outer
`assign` as a free variable, except for the initial `assign` continuation which has no free
variables. I will annotate each constructor field with the name of the free variable that gives
rise to corresponds to that field.

```ocaml
type failure = failure_frame list
and failure_frame = assign * name * env

and assign = assign_frame list
and assign_frame =
  | Neg1
  | Conj1 (* A-2 *) of formula (* e2 *)
  | Conj2 (* A-3 *) of bool (* b1 *)
  | Disj1 (* A-4 *) of formula (* e2 *)
  | Disj2 (* A-5 *) of bool (* b1 *)
```

Due to the list structure, we should keep in mind that when our interpreter examines a `Disj1`, for
example, there will be a sublist of `assign_frame`s as well. This sublist corresponds to the
`assign` free variable present in the original function. Calling `assign` from within an augmented
`assign` continuation in the original program will be translated into a call to `apply_assign` on
the sublist of `assign_frame`s.

### Step 3: Replace calls to unknown functions with calls to `apply`

In this step, we begin to change the implementation of `solve` and `solve_enter`.

```ocaml
let rec solve (r : env) (fail : failure) (phi : formula) (assign : assign) : 'r =
  match phi with
  | Var x -> begin match List.assoc_opt x r with
    | Some b ->
      apply_assign assign r b fail
    | None ->
      apply_assign assign ((x, true) :: r) true @@ (assign, x, r) :: fail
      (* previously: fun () -> assign ((x, false) :: r) false fail *)
    end
  | Neg e ->
    solve r fail e (Neg1 :: assign)
    (* previously: fun r b fail -> assign r (not b) fail *)
  | Conj (e1, e2) ->
    solve r fail e1 (Conj1 e2 :: assign)
    (* previously: fun r b1 fail ->
    if b1 then (* short-circuiting *)
      solve r fail e2 @@ fun r b2 fail -> assign r (b1 && b2) fail
    else
      assign r false fail *)
  | Disj (e1, e2) ->
    solve r fail e1 (Disj1 e2 :: assign)
    (* previously: fun r b1 fail ->
    if b1 then
      assign r true fail
    else
      solve r fail e2 @@ fun r b2 fail ->
      assign r (b1 || b2) fail *)

let solve_enter (phi : formula) : env option =
  solve [] (* the initial environment *)
    [] (* previously: fun () -> None *)
    phi
    [] (* previously: fun r res fail -> if res then Some r else fail () *)
```

### Step 4: Implement `apply`

This step is straightforward. We write recursive functions `apply_failure` and `apply_assign` to
process the lists `failure` and `assign`. Notice that in the 'previously' comments from the above
code, some of the continuations we replaced with constructors will need to call `solve`. This means
that `apply_failure`, `apply_assign` and `solve` will all need to be mutually recursive.

```ocaml

```



[sat-cps]: /posts/2022-10-22-higher-order-continuations.html
[TCO]: https://en.wikipedia.org/wiki/Tail_call

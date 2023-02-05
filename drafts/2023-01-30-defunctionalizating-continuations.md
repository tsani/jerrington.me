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
the

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

type env = (string * bool) list

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

Here's how to do it all at once, using CPS.

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

1. For every type of function that is passed, we define a new type.
   We define one interpreter for each of these types, although at this point all
   we can do is work out the signature of the interpreter, as we haven't decided what the
   constructors of these new types will be.
2. Find every position where we pass an anonymous function.
   Each of those functions becomes a constructor of the datatype corresponding
   to its type.
   Then, we have to identify for each anonymous function all the variables it
   contains that aren't its parameters -- the fancy math-name for those is _free
   variables_.
   The types of the free variables become the fields of the constructor corresponding to the
   function.
4. Find every place where we _call_ a function received through a parameter;
   these will become calls to the `apply` interpreter we will write.
5. Replace every anonymous function with its corresponding constructor generated by the above
   process.

We'll follow these steps one by one in modifying the implementation of `solve`.

1. We define a type `failure` to represent the function `unit -> 'r` and a type
   `assign` to represent the function `env -> bool -> (unit -> 'r) -> 'r`.
   There is a small wrinkle
   Our interpreters will be `apply_failure : unit`
2.

[sat-cps]: /posts/2022-10-22-higher-order-continuations.html
[TCO]: https://en.wikipedia.org/wiki/Tail_call

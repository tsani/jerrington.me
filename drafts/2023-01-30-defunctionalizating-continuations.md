---
title: Functional programmers hate this one trick
---

Originally developed as a technique for implementing compilers for
(higher-order) functional languages, _defunctionalization_ (d17n) is a program
transformation that eliminates higher-order functions.
It is based on the following observation: although there might be infinitely
many possible functions one might pass to a higher-order function, there are
only finitely many functions that are actually used in any given program.
Therefore, we can define a (finite) data type to represent our choice of
function and we can define an interpreter for this data type that recovers the
behaviour of the original function.

One situation we might want to employ d17n as programmers is in
networked applications. In fact, you might have already done this without
realizing it. Higher-order functions give us lots of expressive power, but we
unfortunately can't send functions over the network! So instead, we send a
_representation_ of a function, and the receiving end interprets this
representation to call the right function on its side. For example, we might
send a string that identifies the function to call on the remote side.

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
   Instead of receiving a function as input, it receives a `predicate`,
   which is a _representation_ of a function. *)
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
let divisible_by k l = filter_df (fun x -> x mod k = 0) l
```

Notice that the function `fun x -> x mod k = 0` refers to `k`, which is not a
parameter of the function. To accommodate this, we will add a constructor to our
type `predicate` called `IsDivisible`, and that new constructor will crucially
have one field to store the value of this `k`.

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
quite limiting: it only works for integers. A realistic program will be
filtering many different kinds of lists; perhaps some of integers, some of
strings, and perhaps others of functions! It is possible to further generalize
`predicate` by using GADTs, lifting this apparent limitation of d17n. Alas, a
discussion of GADTs will have to wait for a future article.

## Defunctionalizing continuations

One amazing use of higher-order functions is a technique called
continuation-passing style (CPS). In this style of programming, rather than have
functions that return their results normally, we instead write functions that
"return" their results by calling another function.

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

The challenge is this: devise a way to find a satisfying assignment to the
variables in "one pass". Of course, it can't actually be in one pass because
this is an NP-complete problem; that's why I put quotes around "one pass". What
I mean by "one pass" is that we want to solve this formula without _separately_
enumerating all the possible truth assignments of the variables and evaluating
them.

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

The most interesting and important part of the algorithm is in the `None`
subcase of the `Var` case. In that case, we have encountered a variable for
which we don't have an assigned value in the environment `r : env`.
So we call `assign` with an extended environment _and_ an augmented failure
continuation. It is exactly here that we express the idea "try evaluating the
rest of the tree with `x = true` _but if that fails_ then try evaluating the
rest of the tree with `x = false`."

At first it seems terrifying to try to defunctionalize this, but fortunately,
d17n is a very systematic process. There is exactly one place where we pass an
anonymous function as an argument to `assign` and there are exactly five places
where we pass an anonymous function as an argument to `solve`.
For each of these functions, we need to identify the _free variables_ -- that's
the fancy math-name for variables that aren't parameters.

[sat-cps]: /posts/2022-10-22-higher-order-continuations.html

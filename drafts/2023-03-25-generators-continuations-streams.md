---
title: Implementing generators with continuation-passing style
---

Some languages define a variant of `return` called `yield`. When a function returns normally, it's
finished, but when a function yields, the execution context of the function is saved, enabling us
to re-enter the function to resume its execution. Such resumable functions are especially
convenient for defining lazily generated sequences. Such functions that generate sequences are
called _generators._

This article will first describe native generators in Python before translating the idea into OCaml
using mutable variables and continuation-passing style (CPS). Then, we will see how to eliminate
the mutable variable and how to eliminate the higher-order functions that arise from CPS, leading
to an implementation that we translate into C.

## Native generators in Python

For example, consider this recursive algorithm in Python that enumerates all truth assignments on
`n` variables. Each truth assignment is represented as a list of booleans of length `n`. The
sequence that arises from this enumeration has length `2^n` since there are two possible choices
for the value of each of the `n` variables.

```python
def enumerate_assignments(n):
    a = [True] * n

    def go(i):
        if i == 0:
            yield a
        else:
            a[n - i] = True
            yield from go(i - 1)

            a[n - i] = False
            yield from go(i - 1)

    return go(n)
```

In the base case when `i == 0`, the variable `a` contains a truth assignment with some `True`s and
some `False`s put in it by the stack of recursive calls that leads to the base case. We `yield`
that truth assignment, suspending execution of the function. The function, upon being re-entered,
proceeds from that `yield`, in this case returning back to its caller. The caller might be the
recursive call `yield from go(i - 1)`, after which `a[n - i]` is set to `False` and another
recursive call is made.

Therefore, calling this function does not generate the whole sequence at once. In fact, nothing
happens just yet if we call `enumerate_assignments(5)` except to 'prime' the generator to run. The
call returns a so-called _iterator,_ which stores the state of the execution.  Then, calling the
function `next()` on the iterator will resume execution of the function up to the next `yield`.
Also, `next()` returns to us the yielded value.  Repeatedly calling `next()` until the generator
exits is exactly what a Python `for`-loop does, so we can print all the truth assignments on five
variables like this:

```python
for a in enumerate_assignments(5):
    print(a)
```

What if the language we're working in doesn't have `yield` though? How can we implement something
like this in, say, OCaml? In doing so, we will explore what happens under the hood of generators.

## Evaluation contexts and continuations

Before diving into the implementation of generators in OCaml using continuations, some background
on continuations is required.
I've already written an article on continuations in general [here][higher-order-continuations], but
I'll give a somewhat different take on the core idea in this section.

To motivate the discussion of continuations in this section, let's look ahead at what we will want
to accomplish in the _next_ section: we will implement a generator as a stateful function `next`.
Each time we call this function -- `next ()` -- it returns the next item in the sequence.
The key idea in implementing this function is that just before returning an item in the sequence,
we store the current _evaluation context_ in the reference. Then, when the user of our generator
calls `next ()` again, we restore the saved evaluation context. Evaluation then proceeds from that
saved point up to the next item in the sequence.

I said 'evaluation context' several times in the last paragraph (and in the title of this section)
so it's about time I define what that is and how it's related to continuations.

When a program is "in a call to a function", for instance, there's somewhere that call will return
to. That location corresponds to how the return value of the function is used. Or, said
differently, that location is the _evaluation context._

For example, let's imagine we have a function `f : unit -> int`.
Maybe somewhere in our program there is `let x = f () in E`.
The evaluation context of that call to `f` -- the way the result of `f` is used -- is that it is
associated to the variable `x` in evaluating `E`.
We can write this evaluation context as `let x = _ in E` as this shows where the return value of
the call will be used. Notice that `let x = _ in E` is _not a program!_ Rather, it's a program
_with a hole in it._

Or as another example, perhaps the program contains `f () + 5`.
The evaluation context inside the call to `f` is that the result of the call will be added with
five.
We can write that again with this 'hole' syntax: `_ + 5`.

Of course, evaluation can reach quite deep into a subexpression.
Consider this evaluation context `let x = if _ > 17 then E1 else E2 in E3`.

The relevant insight about evaluation contexts, which are a concept _outside_ our programming
language, is that we can _represent_ them _in_ our programming language: we represent an evaluation
context as a function. Let's see how this applies to the examples seen so far.

* `let x = _ in E` is represented by `fun r -> let x = r in E`
* The second example `_ + 5` is represented by `fun r -> r + 5`
* The last example `let x = if _ > 5 then E1 else E2 in E3`
  is represented by `fun r -> let x = if r > 5 then E1 else E2 in E3`

This functional representation of an evaluation context is called a _continuation_.

Now that we have a way of representing evaluation contexts as continuations, we can write OCaml
programs that manipulate continuations. Since continuations are functions, we cannot inspect them:
the only thing we can do with them is call them. Calling a continuation `k` with an argument `a`
represents filling the hole in the evaluation context represented by `k` with the value `a` and
proceeding to evaluate the resulting expression.

For example, if `k` is the continuation `fun r -> let x = if r > 5 then E1 else E2 in E3` and we
apply this to `3`, then evaluation _continues_ from the point where the value of the hole `_` was
required in the represented evaluation context, namely in computing `_ > 5`. The value of `3 > 5`
is needed in the context `if _ then E1 else E2`, and the value of that if-then-else expression is
needed in the context `let x = _ in E3`. From this apparent nestedness of evaluation contexts, we
can observe that the contexts form a _stack._ This observation will be expanded on considerably in
the last section of this article.

```
    (fun r -> let x = if r > 5 then E1 else E2 in E3) 3
==> let x = if 3 > 5 then E1 else E2 in E3    -- substitute 3 for r
==> let x = if true then E1 else E2 in E3     -- compute '>'
==> let x = E1 in E3                          -- select then-branch
==> ...
```

Great, evaluation contexts can be represented as functions called continuations and our programs
can use continuations by calling them. Calling a continuation corresponds to filling the hole in
the represented evaluation context and continuing the evaluation from there.

The big idea of _continuation-passing style_ (CPS) is to equip the functions we write with an
extra parameter. You guessed it, that extra parameter is the continuation of the function.  So
instead of having a function `f : A -> int` that we use like `f a + 5`, we instead write `f a (fun
r -> r + 5)`. When we implement `f`, we will have access to (a representation of) the evaluation
context in which the call to `f` takes place!

In traditional CPS, functions no longer 'return normally'. Instead, they always return by calling
the continuation. In our current setting of implementing generators, this isn't quite what we want.
We would like our functions to return normally -- this is how the generator will emit a value --
but we nonetheless want access to the evaluation context so that we can store it in a reference
just before returning. In the next section, we'll see how we can transform the Python code above
into OCaml using this form of CPS.

## Implementing generators using state and CPS

In this section, we mimic Python's approach to generators: generators in Python are stateful
objects, so our implementation in this section will use a reference to store an evaluation context.

To come up with our OCaml implementation, let's first translate the Python program from the first
section into pseudo-OCaml with `yield` & `yield from`. Rather than use a mutable array to construct
the truth assignment, we'll build it up one value at a time in a parameter.

```ocaml
let enumerate_assignments n =
    let rec go n a =
        if n = 0 then
            yield a
        else begin
            yield from go (n-1) (true :: a);
            yield from go (n-1) (false :: a)
        end
    in
    go n []
```

Let's concentrate specifically on the inner function `go`. We will need to find a way to represent
`yield` and `yield from` in genuine OCaml. First, let's deal with `yield`.

Operationally, `yield` saves the current evaluation context and emits the given value.
We choose to represent 'emitting a value' as simply returning. To gain access to the current
evaluation context so that we can save it, we rewrite `go` in CPS. We also introduce a `ref` to
store the evaluation context.

```ocaml
let state = ref (* what to put here initially ? *) in
let rec go n a next =
    if n = 0 then
        (state := next; a)
    else begin
        yield from go (n-1) (true :: a);
        yield from go (n-1) (false :: a)
    end
in ...
```

Next, let's address `yield from`. This keyword causes the current generator to invoke a
'sub-generator' and to yield all of its values.

<aside>
In Python, we have the following interpretation of `yield from E`:

```Python
for x in E:
    yield x
```
</aside>

Since we choose to represent yielding a value as simply returning it, we implement `yield from go
...`
as simply calling `go`. In doing so however, we must pass `go` a continuation. We can determine
what continuation to pass by looking at the evaluation context of `yield from go (n-1) (true ::
a)`:

```ocaml
_; yield from go (n-1) (false :: a)
```

We represent this evaluation context as a function: `fun x -> x; yield from go (n-1) (false :: a)`.
Notice that `x : unit`. A value of type `unit` does not convey any information, so can simplify to
`fun () -> yield from go (n-1) (false :: a)`.

Next we must translate the `yield from go ...` that appears inside the continuation. Again,
we translate this simply to a call to `go`, but in doing so we must decide what continuation to
pass in this call. We ask ourselves what evaluation context this call takes place in: what
happens next, after `yield from go (n-1) (false :: a)` finishes generating its sequence of values?
The answer is that `go` returns to its caller. In other words, the evaluation context of `yield
from go (n-1) (false :: a)` is the evaluation context of `go` itself, which was passed to `go` in
the continuation `next`. Therefore, the continuation we pass to this second call to `go` is simply
`next`.

Let's take stock of the translation so far:

```ocaml
let state = ref (* ? *) in
let rec go n a next =
    if n = 0 then
        (state := next; a)
    else
        go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
in ...
```

Let's zoom out, look at `enumerate_assignments` again, and see how we can fit it together with
our adjusted `go`.

```ocaml
let enumerate_assignments n =
    let state = ref (* ? *) in
    let rec go n a next =
        if n = 0 then
            (state := next; a)
        else
            go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
    in
    go n [] (* ? *)
```

But calling `go` right away at the end of `enumerate_assignments` can't be correct anymore. This
will return the first item of the sequence, and then we'll have no way to call the stored
continuation to get the next one!

Instead, we need to adjust the return type of `enumerate_assignments`. Let's look back to the
pseudocode using `yield` and `yield from` to inform how.

```ocaml
let enumerate_assignments n =
    let rec go n a =
        if n = 0 then
            yield a
        else begin
            yield from go (n-1) (true :: a);
            yield from go (n-1) (false :: a)
        end
    in
    go n []
```

First of all, what is _this_ `enumerate_assignments` supposed to return? What is the return type of
`go` in this pseudocode? If we think back to the original Python implementation, it was returning
an _iterator_. This is some kind of object that holds the suspended execution state. We call the
function `next()` on this iterator to resume execution up to the next `yield`.

We can imagine in our OCaml pseudocode that `go` returns something of type `bool list gen` and
therefore that `enumerate_assignments n : bool list gen`. We can further imagine that there's a
function `next : 'a gen -> 'a option` which pumps the generator for one more item. This
hypothetical `next` returns an `option` because the sequence can end and we need a way to
signal this.

Now we need to turn the fantasy of the type `'a gen` and its associated function
`next : 'a gen -> 'a option`
into reality. Here there are several approaches available to us, but one particularly clean one
using higher-order functions is to represent `'a gen` as `unit -> 'a option`. Then, `next` becomes
trivial to implement.

```ocaml
let next f = f ()
```

Hence, we implement `enumerate_assignments` to return a function `unit -> bool list option`, such
that each time this function is called, it emits the next item in the sequence.

```ocaml
let enumerate_assignments n =
    let state = ref (* ? *) in
    let rec go n a next =
        if n = 0 then
            (state := next; a)
        else
            go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
    in
    (* previously: go n [] *)
    fun () -> ???
```

We're almost finished now. What's left is to make it so that the first time the returned function
is called, it emits the first item in the sequence.

The missing insight has to do with the initial value of `state`: it should be a function that makes
the initial call `go n []`. This makes it so that the 'driver function' returned by
`enumerate_assignments` can be implemented as `fun () -> !state ()`. Recall that the state stores a
function that generates the next item in the sequence, so initially we store the function that
generates the first item of the sequence `fun () -> go n []`. Moreover, we implemented `go` to save
the current continuation back into the `state` just before it returns. Therefore, the next time the
driver function is called, it will emit the next item in the sequence!

But there's a small wrinkle in `fun () -> go n []`: there's an argument missing! What continuation
do we pass in this initial call to go?

This continuation will end up saved by `go` in `state` after the whole sequence of truth
assignments is generated. Therefore, we need to arrange that when this continuation ends up stored,
the driver function returns `None`. The driver function we want has the form `fun () -> !state ()`,
so this initial continuation passed to `go` ought to be `fun () -> None`.

But previously, our continuation had the type `unit -> 'a`, so we need to adjust `go` slightly to
accommodate this change. This leads to our finalized generator using CPS and state.

```ocaml
let enumerate_assignments n =
    let rec go n a next =
        if n = 0 then
            (state := next; a)
        else
            go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
    and state = ref (fun () -> go n [] (fun () -> None))
    in
    fun () -> !state ()
```

Now the state variable, which initially contains a call to `go`, must be mutually recursive with
`go`, which refers back to the state variable.

Let's witness the fruits of our handiwork. Here's an OCaml REPL demonstrating the generator.

```
> let next = enumerate_assignments 5 ;;
val next : unit -> bool list option = <fun>

> next () ;;
- : bool list option = Some [true; true; true; true; true]

> next () ;;
- : bool list option = Some [false; true; true; true; true]

> next () ;;
- : bool list option = Some [true; false; true; true; true]

> next () ;;
- : bool list option = Some [false; false; true; true; true]
```

In the next section, we explore how to eliminate the mutable variable from this implementation.
This will give rise to an implementation suitable in purely functional languages, which lack
(genuine) mutable variables.

## Eliminating state

The implementation we have arrived at can be made stateless. Notice that there are two ways that
the driver function `fun () -> !state ()` can return: it can return `Some a` after storing the
continuation, or it can return `None` which happens when the sequence ends and the initial
continuation `fun () -> None` has been stored in the variable `state`.

Rather than store the continuation in some hidden stateful variable, we can simply return both the
assignment _and_ the continuation in the `Some` case. That gives rise to the following intuitive
implementation.

```ocaml
let enumerate_assignments n =
  let rec go n a next =
    if n = 0 then
      Some (a, next) (* return the value _and_ the continuation *)
    else
      go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
  in
  go n [] (fun () -> None)
```

Slight problem with this implementation: it doesn't typecheck! And the error is not a simple
"expect this type, got this other type," but rather

```
Error: The expression `go (n-1) (false :: a) next`
       has type (bool list * (unit -> 'a)) option
       but an expression was expected of type 'a
       The type variable 'a occurs inside (bool list * (unit -> 'a)) option
```

According to this error, the return type of `go`, which is so far inferred as
`(bool list * (unit -> 'a)) option` (due to the expression `Some (a, next)`) has to equal the
return type of `next` which is so far inferred as `'a`. This circularity is forbidden, so OCaml
rejects the program.

We can resolve this by introducing a recursive type, let's say `L`,
such that `L = (bool list * (unit -> L)) option`. The base case of this recursive type arises from
the `None` constructor of the `option` type. Now we can fix the circular variable type variable
`'a` to be `L` and eliminate the forbidden circular instantiation.

```ocaml
type l = Next of (bool list * (unit -> l)) option
```

(Notice that the constructor `Next` witnesses the recursive equality
`L = (bool list * (unit -> L)) option`. We see `Next : (bool list * (unit -> l)) option -> l`
witnessing one direction of the equality, and since `l` has only one constructor, pattern matching
on a value of type `l` witnesses the other direction.)

We can slightly refactor this type by introducing a second constructor and eliminating the
`option`.

```ocaml
type l =
  | Done
  | More of bool list * (unit -> l)
```

And we can generalize the type by replacing `bool list` with a type variable.

```ocaml
type 'a l =
  | Done
  | More of 'a * (unit -> 'a l)
```

And would you look at that! This is simply a list, but whose tail is computed by a function `unit
-> 'a l` rather than being already materialized.

Now we're equipped to rewrite `enumerate_assignments` but having the type
`int -> unit -> bool list l`.

```ocaml
let enumerate_assignments n =
  let rec go n a next =
    if n = 0 then
      More (a, next) (* return the value _and_ the continuation *)
    else
      go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
  in
  fun () -> go n [] (fun () -> Done)
```

Have we achieved our goal of making `enumerate_assignments` stateless? Yes and no.

Indeed we have eliminated the mutable variable, so on the one hand we can say "mission
accomplished." But on the other hand, the state of the walk through the space of truth assignments
is still very much present in our program. That state is captured in the continuation, which is
returned explicitly via the `More` constructor of our _lazy list_ type `l`. The state of our
generator implementation is no longer mutable and hidden, but rather immutable and explicitly
passed around. We can therefore view lazy lists as _purely functional generators._

Another consideration is that in order to make this approach work in a strongly and statically
typed setting as in OCaml, we did need to introduce the recursive type `l`. In the setting of a
different type system, this might not be necessary. For instance, in a dynamically-typed setting,
e.g. in Python, it is unnecessary to introduce an extra type: we can simply return `None` when the
sequence ends and `(a, next)` when the sequence continues.

In the next section, we revisit our implementation using hidden, mutable state, and eliminate from
it the higher-order functions. This will give rise to a first-order implementation suitable for
translation into a language such as C, which is moreover well-equipped to handle mutable state.

## Defunctionalizing the continuation of a generator

Recall from the first section the `enumerate_assignments` implementation using mutable state.

```ocaml
let enumerate_assignments n =
  let rec go n a next =
    if n = 0 then
      (state := next; Some a)
    else
      go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)

  and state = ref (fun () -> go n [] (fun () -> None))
  in
  fun () -> !state ()
```

We can apply _defunctionalization_ to eliminate the higher-order functions present in this program
(see [here][d17n]). In short, we replace each function type $T$ that occurs in the program with a
new datatype $D(T)$. Then, for each function
$\Gamma \vdash \text{fun}\, x \to e_i : T_1 \to T_2$, define a
constructor $C_i : P(\Gamma) \to D(T)$ where $P(x_1 : S_1, \ldots, x_n : S_n) = (S_1, \ldots,
S_n)$. Next, define the function $\text{apply}\, : D(T) \to T1 \to T2$ as follows (in pseudo-OCaml)

```ocaml
let apply (f : D(T)) (x : T1) : T2 = match f with
  | C_i (x1, ..., xN) -> e_i
```

In other words, the function `apply` takes the _representation_ $f : D(T)$ of the original
function and recovers the original function: notice that $\text{apply}\, f : T_1 \to T_2$ has the
original function's type!

For `enumerate_assignments`, we introduce a type `stack` to represent the function type
`unit -> bool list option`.
There are three functions of this type passed as arguments to other functions.

* `fun () -> go n [] (fun () -> None)`: this is the initial continuation and it contains a free
  variable `n : int`. We generate from this a constructor `Start : int -> stack`.
* `fun () -> None`: this is the final continuation and it contains no free variables. We generate a
  constructor `Finished : stack`
* `fun () -> go (n-1) (false :: a) next`: this is the continuation passed when making a recursive
  call to `go`. It has the free variables `n : int`, `a : bool list`, and
  `next : unit -> bool list option`.
  Notice that the function we're translating refers to a function of the type we're
  defunctionalizing. This will make our type `stack` into a recursive type. Moreover, since the new
  continuation defined by this anonymous function refers to exactly one other continuation, we are
  finally justified in calling our type "stack": these continuations were implicitly forming a
  linked list that we now explicitly represent.
  From this analysis we generate the constructor `Continue : int * bool list * stack -> stack`.

We will make a small refactoring: we will separate the "stack frames" from the stack by introducing
a type `frame` and changing the constructor `Continue` to instead have the type
`Continue : frame * stack -> stack`.

```ocaml
type frame = { n : int; a : bool list }
type state =
  | Start
  | Continue of frame * state
  | Finished
```

Equipped with this representation of the functions of type `unit -> bool list option` occurring in
the program, we can translate `enumerate_assignments` to use these constructors instead of using
anonymous functions. Anywhere we _call_ an anonymous function, we instead call the function `apply`
that we implement to recover the behaviour of the anonymous function.
The parameter `next : unit -> bool list option` becomes `s : stack`.

```ocaml
let enumerate_assignments n =
  let state = ref (Start n) in
  (* ^ previously: fun () -> go n [] (fun () -> None) *)
  let rec go n a s =
    if n = 0 then
      (state := s; Some a)
    else
     go (n-1) (true :: a) (Continue ({n; a}, stk))
     (* ^ previously: fun () -> go (n-1) (false :: a) next *)
  (* Apply pops a frame from the stack and runs until the next item is produced, if any.
     When `go` runs, it will manipulate the stack, saving it into `state` in particular right
     before returning `Some a`. *)
  and apply s = match s with
    | Start n -> go n [] Finished
    | Continue ({n; a}, s) -> go (n-1) (false :: a) s
    | Finished -> None
  in
  (* And now we have implemented a function `unit -> bool list option` without using any
     higher-order functions internally! *)
  fun () -> apply !state
```

This program is now completely first-order with the exception of the "higher-order interface"
provided at the very end in the form of the function `fun () -> apply !state`.

This first-order nature will make it possible for us to (somewhat) straightforwardly translate this
into C. Of course, we can translate it "as is", which would mean using a linked list structure for
the type `stack` and for the type `bool list`, but since we're going lower-level, we may as well
choose more efficient representations for these types too.

### Translating to C

The resulting C program is around 100 lines of code whereas the defunctionalized OCaml program
is around 20 lines of code. (Both counts ignore blank lines and comments.)

Our first choice for efficient data representation will be to represent `bool list` as simply
`uint64_t`. This limits the number of variables to 64, but it also has the nice property that on a
64-bit machine such as most, a truth assignment will fit into a register.

[d17n]: /posts/2023-02-12-defunctionalizing-continuations.html
[higher-order-continuations]: /posts/2022-10-22-higher-order-continuations

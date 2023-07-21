---
title: Implementing generators with continuation-passing style, streams, and defunctionalization
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
we store the current _evaluation context_ in a reference. Then, when the user of our generator
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
we can only construct them and call them. Calling a continuation `k` with an argument `a`
represents filling the hole in the evaluation context represented by `k` with the value `a` and
proceeding to evaluate the resulting expression.

For example, if `k` is the continuation `fun r -> let x = if r > 5 then E1 else E2 in E3` and we
apply this to `3`, then evaluation _continues_ from the point where the value of the hole `_` was
required in the represented evaluation context, namely in computing `_ > 5`. The value of `3 > 5`
is needed in the context `if _ then E1 else E2`, and the value of that if-then-else expression is
needed in the context `let x = _ in E3`. From this apparent nestedness of evaluation contexts, we
can observe that the contexts form a _stack._ This observation will be expanded on considerably in
the last section of this article when we translate these ideas into C.

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
r -> r + 5)`. The upshot is that in the implementation of `f`, we will have access to (a
representation of) the evaluation context in which the call to `f` takes place!

In traditional CPS, functions no longer 'return normally'. Instead, they return by calling the
continuation with the value they want to return. In our current setting of implementing generators,
this isn't quite what we want.  We would like our functions to return normally -- this is how the
generator will emit a value -- but we nonetheless want access to the evaluation context so that we
can store it in a reference just before returning. This will enable us to resume the function from
the point where it emitted a value. In the next section, we'll see how we can transform the Python
code from the beginning of the article into OCaml using this form of CPS.

## Implementing generators using state and CPS

In this section, we mimic Python's approach to generators: generators in Python are stateful
objects, so our implementation in this section will use a reference to store an evaluation context.

To come up with our OCaml implementation, let's first translate the Python program from the first
section into pseudo-OCaml with `yield` & `yield from`. Rather than use a mutable array to construct
the truth assignment, we'll build it up one value at a time in a parameter.

<aside>

Since the function we will eventually write is tail-recursive, the list pointer in this parameter
will actually be mutated. Although this isn't the same as using a genuinely mutable array, there
will be some mutation in the resulting program.

</aside>

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
Since `yield from` does not compute anything -- it performs an _effect_ -- we observe that
`x : unit`. A value of type `unit` does not convey any information, so we simplify to `fun () ->
yield from go (n-1) (false :: a)`.

Next we must translate the inner `yield from go ...` which appears inside the continuation. Again,
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

Calling `go` right away at the end of `enumerate_assignments` can't be correct anymore. This
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
using higher-order functions is to represent `'a gen` as a function `unit -> 'a option`. Then,
`next` becomes trivial to implement.

```ocaml
let next f = f ()
```

Hence, we implement `enumerate_assignments` to return a function `unit -> bool list option`, such
that each time this function is called, it emits the next item in the sequence.

<aside>

This analysis _identifies_ the generator with its `next` function. Since the thing we care to do
to a generator is to call `next` on it, we can represent the generator itself with such a function.

</aside>

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
driver function is called, it emits the next item in the sequence!

But there's a small wrinkle in `fun () -> go n []`: there's an argument missing! What continuation
do we pass in this initial call to go?

The continuation passed here ends up saved by `go` in `state` after the whole sequence of truth
assignments is generated. Therefore, we need to arrange that when this continuation ends up stored,
the driver function returns `None`. The driver function has the form `fun () -> !state ()`, so this
initial continuation passed to `go` ought to be `fun () -> None`.

But previously, the continuation had the type `unit -> 'a`, so we need to adjust `go` slightly to
accommodate this change. This leads to the finalized generator using CPS and state.

```ocaml
let enumerate_assignments n =
    let rec go n a next =
        if n = 0 then
            (* we wrap the next item in Some *)
            (state := next; Some a)
        else
            go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
    and state = ref (fun () -> go n [] (fun () -> None))
    (* and arrange that the last continuation to be stored in
       `state` just returns `None`. *)
    in
    fun () -> !state ()
```

Now the state reference, which initially contains a call to `go`, must be mutually recursive with
`go`, which refers back to the state reference.

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
This will give rise to an implementation suitable to purely functional languages, which lack
(genuine) mutable variables.

## Eliminating state

The implementation we have arrived at can be made stateless. Notice that there are two ways that
the driver function `fun () -> !state ()` can return: it can return `Some a` after storing the
continuation, or it can return `None` which happens when the sequence ends and the initial
continuation `fun () -> None` has been stored in the variable `state`.

Rather than store the continuation in some hidden stateful variable, we can simply return _both_
the assignment _and_ the continuation in the `Some` case. That gives rise to the following
intuitive implementation.

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

<aside>

Notice that the constructor `Next` witnesses the recursive equality
`L = (bool list * (unit -> L)) option`. We see `Next : (bool list * (unit -> l)) option -> l`
witnessing one direction of the equality, and since `l` has only one constructor, pattern matching
on a value of type `l` witnesses the other direction.

</aside>

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
(see [here][d17n]). In short, we replace each function type $T = T_1 \to T_2$ that occurs in the
program with a new datatype $D(T)$. Then, for each function
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

Concretely for `enumerate_assignments`, we introduce a type `stack` to represent the function type
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
  linked list that now is explicitly represented.
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
     go (n-1) (true :: a) (Continue ({n; a}, s))
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
is around 20 lines of code. (Both counts ignore blank lines and comments.) We need the following
includes in this development.

```c
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
```

Our first choice for efficient data representation will be to represent `bool list` as simply
`uint64_t`. This limits the number of variables to 64, but it also has the nice property that on a
64-bit machine such as most, a truth assignment fits into a register.
We will need to define some operations for setting and clearing specific bits.

```c
typedef uint64_t truth_assignment;
typedef uint8_t var_index;

truth_assignment set_true(truth_assignment a, var_index i) {
    return a | (1UL << i);
}

truth_assignment set_false(truth_assignment a, var_index i) {
    return a & ~(1UL << i);
}

truth_assignment const EMPTY_TRUTH_ASSIGNMENT = 0;
```

Next, we will translate the type `frame` from the OCaml implementation into a simple C struct.

```c
struct frame {
    var_index i;
    truth_assignment a;
};

struct frame make_frame(var_index i, truth_assignment a) {
    return (struct frame) {
        .i = i,
        .a = a,
    };
}
```

Next, recall the `stack` type from the OCaml implementation.

```ocaml
type stack =
    | Start of int
    | Continue of frame * stack
    | Finished
```

We will represent the linked list structure as a simple array of `struct frame`s. Notice that the
depth of recursion is bounded by the parameter `n` given to `enumerate_assignments`. The maximum
value of the parameter `n` in the development here is `64`. The maximum recursion depth informs the
maximum stack size.

```c
var_index const MAX_VARS = 64;
var_index const STACK_LIMIT = MAX_VARS;
```

To manage this array of frames, we will need to track a _frame pointer:_ this is the index of the
next unused frame in the stack. Seen differently, the frame pointer is the count of frames
currently in the stack.

The frame pointer, being at least 8 bits wide, can accommodate values greater than the count of
frames we will ever store.
This means we can use the upper bits of the frame pointer to help identify what state the generator
is in.
When the generator is in the start state, the stack is empty, so we can use the value
`1 << (WIDTH-1)` for the frame pointer to signify that the generator is in the start state.

In the `Start` state, we need to know the count `n` of variables we're enumerating truth
assignments for, but afterwards we can forget this `n` and just keep the array of frames.
This suggests using a `union` to reuse space here.

```c
struct generator {
    uint8_t frame_pointer;
    union {
        var_index num_variables;
        struct frame stack[STACK_LIMIT];
    } data;
};

enum state {
    START = 1,
    CONTINUE = 0,
};

enum state generator_state(struct generator *gen) {
    // extract the uppermost bit of the frame pointer
    return gen->frame_pointer >> 7;
}

// constructs a generator's initial state
struct generator enumerate_assignments(var_index num_variables) {
    return (struct generator) {
        .data.num_variables = num_variables,
        .frame_pointer = 1 << 7,
    };
}
```

Next, we need operations to push to and pop from the stack held in a `generator`.

```c
/**
 * Returns -1 if pushing fails: wrong generator state or stack is full.
 * Otherwise copies the given frame into the top of the stack, increments the frame pointer,
 * and returns 1. */
int gen_stack_push(struct generator * gen, struct frame const * frame) {
    if (gen->frame_pointer >= STACK_LIMIT) {
        return -1;
    }
    gen->data.stack[gen->frame_pointer++] = *frame;
    return 1;
}

/**
 * Returns -1 if popping is forbidden: wrong generator state.
 * Returns 0 if the stack is empty.
 * Otherwise decrements the frame pointer, and copies the top frame into `out`,
 * and returns 1 */
int gen_stack_pop(struct generator * gen, struct frame * out) {
    if (START == generator_state(gen)) return -1;
    if (0 == gen->frame_pointer) return 0;

    *out = gen->data.stack[-- gen->frame_pointer];
    return 1;
}
```

Now that we've translated all the type definitions, we can translate the programs `go` and `apply`
from OCaml into C. Recall the OCaml implementation:

```ocaml
let enumerate_assignments n =
    let state = ref (Start n) in
    let rec go n a s =
        if n = 0 then
            (state := s; Some a)
        else
            go (n-1) (true :: a) (Continue ({n; a}, s))
    and apply s = match s with
        | Start -> go n [] Finished (* Finished comes from fun () -> None *)
        | Continue ({n; a}, s) -> go (n-1) (false :: a) s
        | Finished -> None
    in
    fun () -> apply !state
```

Notice that `go` refers to the variable `state` that is not a parameter of `go`. In other words,
the definition of `go` constructs a _closure._ Sadly, C does not have closures, so we will
implement this by passing our translation of `go` a pointer to the `generator`. This way when `go`
makes a recursive call, it can simply use `gen_stack_push` to implement the expression `Continue
({n; a}, s)` at the same time as `state := s`. In other words, rather than building up the stack in
a parameter to save it just before returning, our translation of `go` will be mutating the stack
along the way.

Observe also that `go` is _tail-recursive:_ the recursive call is the last thing the function does.
The OCaml compiler transforms this into a while-loop during compilation. This transformation is
called _tail-call optimization._ We will also perform this transformation in our translation, to
avoid using the C call stack.

```c
int go(struct generator * gen, var_index i, truth_assignment * ta) {
    struct frame frame;

    // due to the --> 'operator',
    // `i` will have its value decremented by one inside the loop
    while (i --> 0) {
        *ta = set_true(*ta, n);

        // in the OCaml program, the frame that gets pushed by the recursive call
        // `go (n-1) (true :: a) (Continue ({n; a}, s))`
        // stores the value `n`, but here we are storing n-1 as a consequence
        // of the decrement that happens in the while loop condition.
        frame = = make_frame(n, *ta);
        if(-1 == gen_stack_push(gen, &frame)) return -1;
    }
    return 1;
}
```

Notice that `go` returns a status code here, whereas the original OCaml program returned the truth
assignment. The C program 'returns' the truth assignment via the pointer parameter `ta`, and rather
than construct a new truth assignment, it simply modifies the given one.

Finally, we can translate `apply`. We will call it `next` in the C program, since it will dispatch
on the current generator state to compute the next item in the sequence, updating the generator
state.

```c
int next(struct generator * gen, truth_assignment * ta) {
    int status;
    struct frame frame;

    switch (generator_state(gen)) {
    case CONTINUE:
        status = gen_stack_pop(gen, &frame);

        // handle generator exit
        if (0 == status || -1 == status) return status;

        *ta = set_false(frame.a, frame.i);
        if (-1 == go(gen, frame.i, ta)) return -1;
        return 1;

    case START:
        *ta = EMPTY_TRUTH_ASSIGNMENT;
        gen->frame_pointer = 0;
        if(-1 == go(gen, gen->data.num_variables, ta)) return -1;
        return 1;
    }
}
```

And what good is all this code if we don't try it out. Here's a `main` function to run the
generator until it exits, printing out the truth assignments along the way.

```c
int main() {
    struct generator gen = enumerate_assignments(5);
    int status = 0;
    truth_assignment a;

    for (; status = next(&gen, &a), 1 != status;) {
        printf("truth assignment: %d\n", a);
    }

    if (-1 == status) {
        printf("Generator encountered an error, sorry.\n");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
```

Collecting all this C code into a file `enumerate.c`, we can witness the fruits of our labour:

```bash
$ gcc enumerate.c && ./a.out
truth assignment: 31
truth assignment: 30
truth assignment: 29
truth assignment: 28
truth assignment: 27
truth assignment: 26
truth assignment: 25
truth assignment: 24
truth assignment: 23
truth assignment: 22
truth assignment: 21
truth assignment: 20
truth assignment: 19
truth assignment: 18
truth assignment: 17
truth assignment: 16
truth assignment: 15
truth assignment: 14
truth assignment: 13
truth assignment: 12
truth assignment: 11
truth assignment: 10
truth assignment: 9
truth assignment: 8
truth assignment: 7
truth assignment: 6
truth assignment: 5
truth assignment: 4
truth assignment: 3
truth assignment: 2
truth assignment: 1
truth assignment: 0
```

And there you have it: the most complicated program you've ever seen to count down from `2^n -1`.

## Conclusion

This article covered a lot of ground.

At first, we were motivated by Python's elegant generator syntax and curious about how these ideas
are implemented. We implemented the idea of stateful generators using continuation-passing style to
give us access to a representation of the evaluation context, so we could store that context in a
mutable variable. That gave us the following implementation.

```ocaml
let enumerate_assignments n =
    let rec go n a next =
        if n = 0 then
            (* we wrap the next item in Some *)
            (state := next; Some a)
        else
            go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
    and state = ref (fun () -> go n [] (fun () -> None))
    (* and arrange that the last continuation to be stored in
       `state` just returns `None`. *)
    in
    fun () -> !state ()
```

In the following section, we explored a purely functional take on this idea, motivated by the
simple idea of returning the continuation together with the generated value. To make this
typecheck, we needed to introduce the following recursive type.

```ocaml
type 'a l =
  | Done
  | More of 'a * (unit -> 'a l)
```

This recursive type (or some variant thereof) is often presented in programming languages courses
simply as "a lazy list". The development in this article, on the other hand, _derived_ this
representation by eliminating the mutable variable from the program in the previous section.
This demonstrates that lazy lists are purely functional generators, where the state of the
generator is captured in the continuation of type `unit -> 'a l` that is explicitly returned.

In the following section, we applied defunctionalization to the stateful CPS generator to convert
it into a state machine using an explicit stack.

```ocaml
let enumerate_assignments n =
  let state = ref (Start n) in
  let rec go n a s =
    if n = 0 then
      (state := s; Some a)
    else
     go (n-1) (true :: a) (Continue ({n; a}, s))
  and apply s = match s with
    | Start n -> go n [] Finished
    | Continue ({n; a}, s) -> go (n-1) (false :: a) s
    | Finished -> None
  in
  fun () -> apply !state
```

In the final section, we translated the defunctionalized program into C, using efficient data
representations along the way where possible.

I hope that this sheds some light on the connection between CPS, generators, and state machines.

[d17n]: /posts/2023-02-12-defunctionalizing-continuations.html
[higher-order-continuations]: /posts/2022-10-22-higher-order-continuations

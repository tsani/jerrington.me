---
title: Implementing generators with continuation-passing style
---

Some languages define a variant of `return` called `yield`. When a function returns normally, it's
finished, but when a function yields, the execution state of the function is saved, enabling us to
re-enter the function to resume its execution. Such resumable functions are especially convenient
for defining lazily generated sequences. Such functions that generate sequences are called
_generators._

This article will first describe native generators in Python before translating the idea into OCaml
using mutable variables and continuation-passing style (CPS). Then, we will see how to eliminate
the mutable variable and how to eliminate the higher-order functions that arise from CPS, leading
to an implementation that could be translated straightforwardly into a lower-level, first-order
language.

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
first recursive call `yield from go(i - 1)`, after which `a[n - i]` is set to `False` and another
recursive call is made.

Therefore, calling this function does not generate the whole sequence at once. In fact, nothing
happens just yet if we call `enumerate_assignments(5)` except to 'prime' the generator to run. The
call returns an _iterator,_ which stores the state of the execution.  Then, calling the function
`next()` on the iterator will resume execution of the function up to the next `yield`.  Also,
`next()` returns to us the yielded value.  Repeatedly calling `next()` until the generator exits is
is exactly what a Python `for`-loop does, so we can print all the truth assignments on five
variables like this:

```python
for a in enumerate_assignments(5):
    print(a)
```

What if the language we're working in doesn't have `yield` though? How can we implement something
like this in, say, OCaml?

## Implementing generators using state and CPS

Our goal will be to implement a function `enumerate_assignments : int -> unit -> bool list option`
such that `enumerate_assignments n` returns a function `next` such that `next ()` returns the next
truth assignment in the sequence wrapped in `Some`. If the sequence is finished, then it will
return `None`.

Clearly, the function `next` computed by `enumerate_assignments n` is impure as it returns
different outputs for the same input. The impurity comes from the fact that we use a mutable
variable to hold the execution state of the recursive algorithm. We access the execution state by
writing the recursive algorithm in some form of continuation-passing style (CPS). In doing so, we
extend `go` with an additional parameter called a _continuation_ and that I name `next`.  The
continuation is a _function_ that holds the work to do after the call completes.

Let's build up this generator-using-CPS idea in stages. First, let's write a recursive algorithm
similar to `go` above in continuation-passing style in OCaml.

```ocaml
let rec go n a next =
  if n = 0 then
    (* missing: store `next` into a variable *)
    Some a (* 'yield' the truth assignment `a` *)
  else
    go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
```

This algorithm builds up the truth assignment one value at a time in the parameter `a`. When `n =
0`, the assignment is complete and is returned wrapped in `Some`. What's missing is to store the
continuation in a variable before we return the assignment and to introduce a "driver function"
When `n > 0`, we make a recursive call extending the current assignment with `true` and
extendeding the continuation with a (suspended) recursive call that instead extends the current
assignment with `false`.

Now let's wrap the definition of `go` into a function `enumerate_assignments`. It's this function
that will define the variable that stores the continuation. When we create this variable, we
must choose an initial continuation: that will be to call `go n [] (fun () -> None)`.
Finally, `enumerate_assignments` will return a function that when called, extracts the function
from the mutable variable and invokes it. Notice that `go` needs to refer to the continuation
variable and the continuation variable needs to refer to `go`. Therefore, these two definitions
must be mutually recursive.

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
      Some (a, next)
    else
      go (n-1) (true :: a) (fun () -> go (n-1) (false :: a) next)
  in
  go n [] (fun () -> None)
```


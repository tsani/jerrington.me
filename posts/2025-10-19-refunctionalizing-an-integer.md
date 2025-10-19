---
title: Refunctionalizing an integer?
---

First imagine that the OCaml type `int` is infinite. Now what happens when you refunctionalize an
infinite datatype?

Let make a concrete program to work with. This will be a stateful generator that outputs the
integers, starting at `0`, and ending at an exclusive limit `n`.

```ocaml
type 'a gen = { next : unit -> 'a option }
let range n =
    let r = ref 0 in {
        next = fun () ->
            let i = !r in
            if i < n then
                Some (r := i+1; i)
            else
                None
    }
```

This program stores an integer in a reference cell, and branches on it to produce two different
behaviours. Our goal is to replace the integer in the cell with a function in the cell that will
just do the right thing (i.e. perform the right one of these two behaviours) without needing to
branch on the integer to decide what to do.

We will need to store some initial function inside the reference. This function, when called, needs
to emit `0` and also update its own reference cell to be the function that will emit `1`. But this
new function must also do more: when _it's_ called it also needs to update its own reference to be
the function that emits `2`, and so on.

So I lied: we're not going to _get rid_ of the integer, as we would expect from a standard
r17n. We just won't be storing it directly. We'll be storing it indirectly, somehow.

To see how we'll store this integer, let's begin translating the function, leaving blanks for the
parts we aren't sure about yet.

```ocaml
let range n =
    let r = ref (fun () -> ???; 0) in
    { next = fun () -> !r () }
```

What must go in the question marks is our logic to update the reference cell to be the "next"
function in the sequence. Let's encapsulate this update logic into a function `update`. Crucially
this function will take an integer as input, to identify the next function in the sequence. To
identify the next function to store, the update function must decide whether the real sequence, of
integers, is finished. In that case, it stores a function that simply outputs `None`. If the
real sequence isn't finished, then `update` must store a function that will contain a recursive
call to `update`, but on the next integer.

```ocaml
let range n =
    let rec r = ref (fun () -> update 1; 0)
    and update i =
        if i < n then
            r := fun () -> update (i+1); Some i
        else
            r := fun () -> None
    in
    { next = fun () -> !r () }
```

There is a slight issue with this implementation: it's incorrect on an edge case. How about `range
0`? This generator should start out by returning `None`, but our implementation would have any
generator constructed from `range n` begin by emitting `0` unconditionally.

The insight we need to patch this up is to recognize that we can make the initial function stored
in the reference cell begin by calling `update 0`. This will check whether `0` is in bounds before
proceeding to store the appropriate function in the reference. Therefore, our new initial function,
after calling `update 0`, can then dereference `r` to obtain the _new_ handler, for the `0` case,
and call it.

```ocaml
let range n =
    let rec r = ref (fun () -> update 0; !r ())
    and update i =
        if i < n then
            r := fun () -> update (i+1); Some i
        else
            r := fun () -> None
    in
    { next = fun () -> !r () }
```

Indeed, the integer is still alive and well, but it isn't what we store in the reference cell
anymore, at least not directly. Instead, we store the closure `fun () -> update (i+1); Some i`,
which has captured the integer `i`.

Did our transformation in any way _improve_ the program we wrote? No way. But you have to agree,
there's something mysterious and exciting about using a mutable variable to store a recursive
function that updates the mutable variable that houses that function.

Getting back to the title of this post now, did we really refunctionalize an integer? I'd actually
say we did. R17n would have us write down one function for each of the values in the first-order
datatype we're refunctionalizing. Doing this for a datatype with several billion values, like the
integers, is practically infeasible. However, since there were just two _behaviours,_ depending on
the relationship between the index `i` and the limit `n`, there are really just two different
functions we need to store. One of those is `fun () -> None`, and the other is `fun () -> update
(i+1); Some i`. This latter code snippet, having a free variable `i`, actually determines a
_family_ of functions indexed by `i`. For a fixed limit `n`, we implemented the function `update`
that maps each integer to a function that simulates the behaviour in the original program.

Taking a more general stance, when we attempt to refunctionalize a datatype with
infinitely many values -- or with so many values that we can approximate the count to be infinite
-- we end up needing to describe a systematic way of translating each of the values into a
corresponding function. In other words, we implement a function to translate each value into its
refunctionalized form.

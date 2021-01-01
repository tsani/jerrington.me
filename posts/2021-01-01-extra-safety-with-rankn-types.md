---
title: A little extra safety with RankNTypes
---

In solving [AoC 2020 day 20](https://adventofcode.com/2020/day/20), I found a
neat use for RankNTypes that I anticipate using again when I can.

To see what RankNTypes gives us, let's first look at an ordinary higher-order
function.

```haskell
map :: (a -> b) -> [a] -> [b]
```

This function is _polymorphic_, meaning that its type has some
type variables, namely `a` and `b`. When we call map, we pass not
only two value arguments of type `a -> b` and `[a]`, but also two _type_
arguments `a` and `b`. Usually, this second passing is hidden from us, but there
is [an extension][type-applications] to make it explicit.

Crucially, this means that when we _define_ `map`, the function parameter is not
a polymorphic function! It's an ordinary function, with a somewhat mysterious
type. All we know about it in that context is that the input type of the
function happens to be the same as the element type of the list.

Back to the AoC problem. In part 1, we don't care about the interior of the
tile, but rather only about its edges. And for maximum flexibility, we can
choose not to commit to a particular edge type. This gives a definition like

```haskell
data Tile a = Tile { left :: a, top :: a, right :: a, bottom :: a }
```

It's nothing more than a 4-tuple with a name.

Usually, the variable `a` will be instantiated with a type `Edge` that is the
concrete representation of an edge of a tile, but by using a type variable, we
can separate generic tile transformations (such as rotations and flips) from
more specialized ones that depend on the particulars of the edges.

A crucial part of the solution is an algorithm that given a pair of tiles and a
pair of sides finds a transformation (a rotation + a flip) for the second tile
that brings the edges on those sides of the tiles into alignment.
I chose to represent edges clockwise, like this:

```
a b c d e
p       f
o       g
n       h
m l k j i
```

There are benefits to this representation that I won't get into, but suffice to
say that two edges `e1` and `e2` are aligned in this representation if `e1` is
equal to the reverse of `e2`.

The basic idea of the alignment algorithm is to check if the edges on the
desired sides of the tiles are aligned -- that's the base case, and the output
transformation then is identity -- else, rotate the second tile and repeat.
We could error out (with `Maybe`) in case we rotate 4 times and we still
haven't found an alignment, but instead we'll just say that the alignment
algorithm has a precondition that the alignment has to exist. It merely finds
the alignment.

Now here's the punchline, and why RankNTypes gives us extra safety: a side of a
tile is... _a polymorphic function!_

You might be thinking I'm contradicting myself, because I defined the edges of a
`Tile a` to be of type `a` and there are no functions in sight here. But a
_side_ is what I'm calling a way of _selecting_ an edge from a tile.

```haskell
newtype Side = Side (forall a. Tile a -> a)
```

It's the RankNTypes extension that enables us to write `forall a` here, and the
meaning is that the function contained in this `newtype` is polymorphic.
And there are exactly four (legitimate) values of the type `Side`: each is
built from one of the record selectors `left`, `top`, `right`, and `bottom`.

We're at last ready to write the type of the alignment algorithm.

```haskell
align
  :: (Eq a, Reversible a)
  => (Tile a, Side) -> (Tile a, Side)
  -> Transf a
```

<aside>
Since I want to keep the tiles generic, I needed to introduce a typeclass
`Reversible` since `align` needs to be able to flip edges to check alignment.
</aside>

Now you might be asking yourself: why bother? We could have just written:

```haskell
align
  :: (Eq a, Reversible a)
  => (Tile a, Tile a -> a) -> (Tile a, Tile a -> a)
  -> Transf a
```

And that's completely true. We could have written this. But this way of
expressing `align` falls victim to a possibility of misuse.
Remember how when we called `map` at the beginning of this post, the call site
chose the instantiations for the type variables (even though it happened
implicitly)?
Well suppose we call `align` with a concrete instantiation for `a`, e.g. `Edge`.
Then we need to supply functions of type `Tile Edge -> Edge`. Because we know
that these functions return `Edge`s, we can give implementations that don't
actually return edges of the input tile!
On the other hand, by using the `Side` type, we buy ourselves a little extra
safety at a very low cost: we just need to wrap and unwrap the newtype `Side` to
completely eliminate this potential coding error.

What's more is that this technique is applicable even to functional languages
without the plethora of language extensions that GHC supports.
(Well, at least one other not fully dependently-typed functional language.)
For example, OCaml also lets you pass polymorphic functions inside records.

```ocaml
type side = { side : 'a. 'a tile -> 'a }
```

To sum up, the major takeaway I had from this journey is that it can be hugely
beneficial to avoid committing to representations for as long as possible. By
deciding earlier to define a tile using a type variable, I'm enabled to
define a precise and safe notion of `Side` to select edges.
This is an application of one of the bigger principles of type-driven
development: **make illegal states unrepresentable**.
Going forward, I hope to find equally cheap ways to do just that.

P.S. This article does simplify a few pointsIn case you're interested in the gory details,

[type-applications]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications

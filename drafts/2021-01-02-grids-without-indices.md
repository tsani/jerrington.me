---
title: Grids without indices
---

Working with grids in Haskell (or any purely functional language, I'm sure) can
be pretty painful, but it doesn't have to be. The reason it's so painful is
that we're tempted to manipulate grids using indices.
Now maybe if we're using a nice array library such as `vector`, then index-based
manipulations aren't so gross, but in this article, I'll show you a way to
manipulate grids that is based on good ol' lists, and which generalizes to
structures beyond grids.

A common problem involving grids is to construct a new one based on an old one
by looking in a neighbourhood around each point in the grid. For example,
Conway's [Game of Life][gol], which is what's called a _cellular automaton_, is
such a problem. The idea is that we have a grid, and each cell is either dead or
alive. Each iteration constructs a new grid from the current grid:

* A dead cell becomes alive if it has exactly three live neighbours.
* A live cell remains alive if it has two or three live neighbours.
* Any cell dies or remains dead otherwise.

The challenge with this problem is that each cell is not completely independent:
each cell needs to know something about its neighbours in order to update. The
upshot is that we can't just map over the grid to do an update. We need some
kind of "context-aware" map. The ultimate goal of this article will be to arrive
at exactly such an abstraction.

One dimension
-------------

Yes, Game of Life is flashy, but let's start with a 1D cellular automaton called
[Rule 110][rule-110]. I'll skip the details of this automaton and instead let
you look at this beautiful gif that illustrates how the next generation is
constructed.

![[licence][rule-110-gif-citation]][rule-110-gif]

A 1D grid is just a list, but remember, we have this requirement of some kind of
context awareness. So let's just add a notion of a "focus" to a list. We end up
with a structure that I'll call `Z`.

```haskell
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Z where

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as N

data Z a = Z [a] a [a] deriving (Functor, Foldable, Traversable)

fromList :: NonEmpty a -> Z a
fromList (x :| xs) = Z [] x xs
```

What's cool about this structure is that we can move the focus around. In
particular, we can move it to the left, or to the right, provided there's still
stuff in the relevant list.

```haskell
left :: Z a -> Maybe (Z a)
left (Z [] _ _ ) = Nothing
left (Z (l:ls) x rs) = Just (Z ls l (x:rs))

right :: Z a -> Maybe (Z a)
right (Z _ _ []) = Nothing
right (Z ls x (r:rs)) = Just (Z (x:ls) r rs)
```

Visually, here's what moving left and right looks like, using `|` to isolate the
focus.

```
Initially:   ... a  b |c| d e ...
Moving left: ... a |b| c  d e ...
```

This isn't particularly mindblowing. But here's what is: what if we collected
_all_ the different possible focuses? In other words, we want a structure that
has a version of this `Z` with `a` selected, and one with `b` selected, and one
with `c` selected, and so on. Then, we can map over this structure in order to
implement Rule 110.

What better structure to use to hold all these different "versions" of our
starting `Z` than to use `Z` itself!

[rule-110-gif]: https://upload.wikimedia.org/wikipedia/commons/b/b5/One-d-cellular-automaton-rule-110.gif
[rule-110-gif-citation]: https://commons.wikimedia.org/wiki/File:One-d-cellular-automaton-rule-110.gif
[rule-110]: https://en.wikipedia.org/wiki/Rule_110
[gol]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

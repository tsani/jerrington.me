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
alive. Each iteration constructs a new grid from the current grid by applying
the following process to each cell simultaneously:

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

In short, each cell looks at at itself and its left and right neighbours to
decide what state it should be in (on or off) in the next generation.

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

We can of course extract the focused element from the `Z`.

```haskell
extractZ :: Z a -> a
extractZ (Z _ x _) = x
```

We can also move the focus around. In particular, we can move it to the left, or
to the right, provided there's still stuff remaining in the relevant list.

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

The resulting structure has the same list inside of it; all that's changed is
our point of view.
Now that isn't particularly mindblowing. But here's what is: what if we
collected _all_ the different possible focuses? In other words, we want a
structure that has a version of this `Z` with `a` selected, and one with `b`
selected, and one with `c` selected, and so on. Then, we can map over this
structure in order to implement Rule 110.

Let's call this "collect all the different versions" operation "`duplicateZ`".
We provide `duplicateZ` with a particular `Z a`, which is focusing on some `a`.
This input `Z a` must appear in the output, since it's one of the ways we can
look at the list that underlies the `Z a`.

To represent the output of `duplicateZ`, let's use the type `Z` itself!
This choice might sound a bit arbitrary at first, but it has some very nice
benefits. First, it explains why we called it `duplicateZ`; have a look at the
type now:

```
duplicateZ :: Z a -> Z (Z a)
```

It's the `Z` that gets duplicated!

To sort out the implementation of `duplicateZ`, let's look at some properties we
would like it to have:

1. The input `Z a` ought to be the focused element of the output, meaning that
   `extract (duplicateZ z)` gives us back the input `z`.
2. Moving left and right should commute with duplication, namely
   `duplicateZ <$> left z` should equal `left (duplicateZ z)` (and likewise for
   `right`).

To implement `duplicateZ` satisfying these properties, we need a way to
collect all the left and right moves from the input. We'll need a helper.

```haskell
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

duplicateZ :: Z a -> Z (Z a)
duplicateZ z =
  Z (tail $ iterateMaybe left z) z (tail $ iterateMaybe right z)
```

Remember the property we expected to hold? Look closely at the implementation
above to convince yourself that it holds. We can generalize the property a bit
more: if we extract from all the positions generated by the duplication, we get
back the original structure. Of course, since `Z` is also a Functor, we can use
`fmap` to extract at every position.

```
fmap extractZ . duplicateZ = id
```

This property is one of the _comonad laws_. Yes, `Z` is what's called a comonad!
The ability to extract from and duplicate the structure in such a way that these
operations 'cancel out' when composed is the essence of what it means to be a
comonad. There is a further operation we can derive from these two, namely a
function to apply a transformation at every possible focused position. We'll
call this operation `extend`.

```haskell
class Functor w => Comonad w where
  duplicate :: w a -> w (w a)
  extract :: w a -> a

  extend :: (w a -> b) -> w a -> w b
  extend k = fmap k . duplicate

instance Comonad Z where
  duplicate = duplicateZ
  extract = extractZ
```

Extend is implemented by duplicating the structure (which collects all the
focuses into a new structure) and then using `fmap` to apply the transformation
everywhere. What's fascinating about `extend` is that it looks _almost_ like
`fmap`. The difference is that the input to the function parameter gets a `w a`
instead of merely an `a`. Concretely for `Z`, what this means is
that the passed function can inspect what's _around_ the focus in order to
compute the `b`. Tying this back to cellular automata, we can use `extend` to
represent the process of simultaneously applying the rule of the automaton to
every position in the strip (or grid, as we'll see in 2D).

<aside>
Let's briefly
look at comonads versus monads, on the level of types.

```haskell
class Applicative m => Monad m where
  pure :: a -> m a
  join :: m (m a) -> m a
  bind :: (a -> m b) -> m a -> m b

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
```

Comonads are dual to monads, as you can see in the types: the operations are all
backwards. (This is also why we often see `w` for a comonad: it's an upside down `m`.)
</aside>

Now let's implement the cellular automaton, Rule 110.
We'll write a function `rule110 :: Z Bool -> Bool` which, given a focused cell,
decides whether that cell should be alive or dead in the next iteration.
To handle the boundaries, we will consider that a cell outside the bounds of the
strip are dead.

```haskell
rule110 :: Z Bool -> Bool
rule110 z = case (get left, extract z, get right) of
  (False, False, False) -> False
  (False, False, True) -> True
  (False, True, False) -> True
  (False, True, True) -> True
  (True, False, False) -> False
  (True, False, True) -> True
  (True, True, False) -> True
  (True, True, True) -> False
  where
    get f = maybe False extract (f z)
```

This function determines whether a particular cell is dead or alive in the next
iteration.
Using `extend`, we can upgrade this to a function that computes the whole next
strip.

```haskell
step :: Z Bool -> Z Bool
step = extend rule110
```

And finally, we can upgrade this stepping function to one that computes, given
an initial strip, the infinite list of its evolution.

```haskell
steps :: Z Bool -> [Z Bool]
steps = iterate step
```

Overall, comonads give us an elegant language to express _contextual_ computations. The computation
of the cellular automaton Rule 110 is contextual: each cell's future state depends not only on its
current state, but also on the current state of both its neighbours.

Two dimensions
--------------

Let's solve a real problem, with an eye towards efficiency. What problem could be more real than
this year's [Advent of Code, Day 4](https://adventofcode.com/2025/day/4)?
It's a problem about grids that look like this.

```
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
```

In part 1, we want to count how many `@` cells have fewer than 4 other `@`s in their eight-square
neighbourhood.

We could -- and I certainly did, in the past -- construct a two-dimensional variation on what we
did above. However, the resulting structure, being built entirely out of linked lists, is
enormously inefficient. Not only that, but it will end up privileging certain movements: if we
designed this grid in a row-major way, then moving to an adjacent row is instant, but
moving to an adjacent column requires shifting _every row._ This is unacceptable.

Instead, let's bust out the trusty `vector` library, with its $O(1)$ indexing. This way,
rather than represent the current focus directly in the structure of the data as we did with `Z`,
we'll store an index.

Sounds like I lied, right? Whatever happened to the "grids without indices"?
As an unfortunate consequence of desiring efficiency, our `Grid` module will house a few index
calculations, but the actual application logic that solves the problem will not. So I only
half-lied.

```haskell
module Grid where

data G a = G !(Vector (Vector a)) !Int !Int -- row and column indices
    deriving Functor

out :: G a -> Vector (Vector a)
out (G v _ _) = v

width, height :: G a -> Int
width (G v _ _) = V.length (v V.! 0)
height (G v _ _) = V.length v
```

Next, we need to write `Comonad` instance for `G` -- this is the interesting part.

- `extract :: G a -> a` will use the underlying vectors' indexing to obtain the focused element.
- `duplicate :: G a -> G (G a)` will construct a grid of grids. This sounds like it will absolutely
  explode the memory usage of the program, but in fact, the underlying grid we start with will be
  shared unchanged among all the new `G` objects we'll create. These objects will merely differ in
  which element is focused, i.e. in what indices they store.
- `extend :: (G a -> b) -> G a -> G b` will function as a fusion of `duplicate` together with an
  `fmap` of the transformation.

```haskell
instance Comonad G where
  extract (G v i j) = v V.! i V.! j

  duplicate g@(G v i j) = G v' i j where
    v' = V.generate (height g) $ \i ->
      V.generate (width g) $ \j ->
        G v i j

  extend f g@(G v i j) = G v' i j where
    v' = V.generate (height g) $ \i ->
      V.generate (width g) $ \j ->
        f (G v i j)
```

Although we could define `extend f = fmap f . duplicate`, I'm unsure of whether this will fuse away
the intermediate grid. Better safe than sorry.

Finally, our Grid module will be complete with some functions for moving the focus around, either
absolutely or relatively.

```haskell
seek :: G a -> (Int, Int) -> Maybe (G a)
seek g@(G v _ _) (i, j)
  | 0 <= i && i < height g
  && 0 <= j && j < width g = Just (G v i j)
  | otherwise = Nothing

move :: G a -> (Int, Int) -> Maybe (G a)
move (G v i j) (di, dj) = seek (G v i j) (di + i, dj + j)

-- the 8 relative offsets for the neighbourhood of interest
dir8 = [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)]
```

Now we're ready to tackle solving part 1 of the coding challenge. Remember, we want to count the
`@`s that have fewer than 4 `@`s in the 8-square neighbourhood around them. We encode this as a
predicate `G Cell -> Bool` that decides whether the focused cell of the grid is an accessible `@`.

```haskell
data Cell = Empty | Full
    deriving Enum -- to convert Empty to 0 and Full to 1

-- Is the focused cell an accessible roll of paper?
accessible :: G Cell -> Bool
accessible g
  | Full <- extract g = (< 4) . sum $ maybe 0 (fromEnum . extract) . move g <$> dir8
  | otherwise = False
```

- `move g <$> dir8` moves in all 8 relative directions given by the list `dir8`. Since this could
  go out of bounds, each result in wrapped with `Maybe`.
- `maybe 0 (fromEnum . extract)` converts a single result into `1` only if the movement was in
  bounds and then focuses on an `@`.

With this predicate defined, we can finally express the solution to part 1.

```haskell
part1 :: Vector (Vector Cell) -> Int
part1 v = go (G v 0 0) where
    go = sum . fmap sum . out . fmap fromEnum . extend accessible
```

- `extend accessible` gives us a grid of booleans each saying whether the cell at the corresponding
  position is an accessible `@`.
- `fmap fromEnum` changes the booleans into integers.
- `sum . fmap sum . out` throws out the focus and adds everything up.

While we're here, we may as well do part2. It's not much harder. We're asked to repeatedly remove
all accessible `@`s from the grid, counting how many we remove in total.

To solve this, let's express a new contextual computation that builds on `accessible`. Rather than
merely map to a boolean, we'll map to a tuple consisting of a count (zero or one) and a new value
for the cell under focus. I have no good name for this, so let's use a greek letter.

```haskell
rho g = if accessible g then (1, Empty) else (0, extract g)
```

Applying this everywhere in the grid via `extend` will give us a grid of tuples. We'll unzip this
to get a grid of ones and zeros -- we add them up to get the count of removed `@`s -- and a new
grid with fewer `@`s in it.

Then, it suffices to iterate this process to produce an infinite stream of counts of removed `@`s
in each iteration. This stream will eventually reach zero, staying there forever, so we'll take the
nonzero prefix and add that all up.

```haskell
part2 :: Vector (Vector Cell) -> Int
part2 v = go (G v 0 0) where
    go = sum . takeWhile (> 0) . unfoldr (Just . phi) where
        phi = first (sum . fmap sum . out) . funzip . extend rho

funzip f = (fst <$> f, snd <$> f)
```

See, I promised there would be no indices in the solution, and I delivered! The resulting solution
crucially uses `extend` to apply a contextually-aware transformation everywhere in the grid. All
index manipulation is relegated to the Grid module's `jump` function. Overall, I find that this
approach to solving problems about grids to be delightfully declarative. Sometimes it's really just
a matter of finding the right abstraction.

P.S.

I originally did most of this development using Haskell's quasi-dependent types. This was necessary
in order to use a [store comonad parameterized by a representable
functor](https://hackage-content.haskell.org/package/adjunctions-4.4.3/docs/Control-Comonad-Representable-Store.html).
Phew. I used this Store comonad because of its touted ability to memoize results. The generality of
this technique cut against me, however, as it required me to find a type to serve as an index into
the grid. `(Int, Int)` didn't cut it, because of the further requirement that there be no "out of
bounds" values in the index type. Well this is where the dependent types came in. I decided to use
bounded natural numbers as indices. This did actually give a workable solution, but it was all
quite disgusting.

It hit me a little later that I didn't need to use this highly general machinery. I could just
define a Comonad instance myself for a 2D vector + coordinates, so that's the approach that ended
up in this article.

[rule-110-gif]: https://upload.wikimedia.org/wikipedia/commons/b/b5/One-d-cellular-automaton-rule-110.gif
[rule-110-gif-citation]: https://commons.wikimedia.org/wiki/File:One-d-cellular-automaton-rule-110.gif
[rule-110]: https://en.wikipedia.org/wiki/Rule_110
[gol]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

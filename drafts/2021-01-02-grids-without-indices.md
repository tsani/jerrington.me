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
to the right, provided there's still stuff in the relevant list.

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
call this operation `cobind`.

```haskell
class Functor w => Comonad w where
  duplicate :: w a -> w (w a)
  extract :: w a -> a

  cobind :: (w a -> b) -> w a -> w b
  cobind k = fmap k . duplicate

instance Comonad Z where
  duplicate = duplicateZ
  extract = extractZ
```

Cobind is implemented by duplicating the structure (which collects all the
focuses into a new structure) and then using `fmap` to apply the transformation
everywhere. What's fascinating about `cobind` is that it looks _almost_ like
`fmap`. The difference is that the input to the function parameter gets a `w a`
instead of merely an `a`. What this means concretely in the context of `Z`, is
that the passed function can inspect what's _around_ the focus in order to
compute the `b`. Tying this back to cellular automata, we can use `cobind` to
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
  cobind :: (w a -> b) -> w a -> w b
```

Comonads are dual to monads, as you can see in the types: the operations are all
backwards. (This is also why I use the letter `w`: it's an upside down `m`.)
</aside>

Now let's implement the cellular automaton, Rule 110.
We'll write a function `rule110 :: Z Bool -> Bool` which, given a focused cell,
decides whether that cell should be alive or dead in the next iteration.
To handle the boundaries, we will consider that a cells outside the bounds of the
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
Using `cobind`, we can upgrade this to a function that computes the whole next
strip.

```haskell
step :: Z Bool -> Z Bool
step = cobind rule110
```

And finally, we can upgrade this stepping function to one that computes, given
an initial strip, the infinite list of its evolution.

```haskell
steps :: Z Bool -> [Z Bool]
steps = iterate step
```




[rule-110-gif]: https://upload.wikimedia.org/wikipedia/commons/b/b5/One-d-cellular-automaton-rule-110.gif
[rule-110-gif-citation]: https://commons.wikimedia.org/wiki/File:One-d-cellular-automaton-rule-110.gif
[rule-110]: https://en.wikipedia.org/wiki/Rule_110
[gol]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

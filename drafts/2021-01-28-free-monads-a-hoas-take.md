---
title: 'Free monads: a HOAS take'
---

Free monads, in the Haskell sense, are a technique for representing a monadic
computation as a data structure. Then, one writes a separate function to
interpret the free monad into a concrete monad in order to specify precisely
what the effects of the monad are. This separates two concerns that are tangled
when programming directly in a concrete monad: the construction of the monadic
program, and the concrete nature of the effects being performed.

Free monad for state
--------------------

Take for example the state monad.

```haskell
newtype State s a = State { runState :: s -> (s, a) }
  deriving Functor

instance Monad (State s) where
  State f >>= k = State $ \s ->
    let (s', x) = f s in runState (k x) s'
```

The state monad has two core operations.

```haskell
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> (s, ())
```

We can turn this idea around: instead of saying that this particular newtype is
_the_ state monad, we can talk about what it means for _a monad_ to be _a state
monad_.
In a nutshell, a monad is a state monad if it can implement `get` and `put` such
that: any `get` after `put s` is the same as `pure s`, and `get >>= put` is the
same as `pure ()`.

And indeed, the `State` monad above is _a_ state monad. To see this, let's prove
the second property we expect of a reasonable state monad:

```haskell
get >>= put
= (State $ \s -> (s, s)) >>= \s -> State $ \_ -> (s, ())
  -- definition of get and put

= State $ \s ->
  let (s', x) = (\s -> (s, s)) s in
  runState ((\s -> State $ \_ -> (s, ())) x) s'
  -- definition of >>=

= State $ \s ->
  runState ((\s -> State $ \_ -> (s, ()) s)) s
  -- function application, let-elimination

= State $ \s ->
  runState (State $ \_ -> (s, ())) s
  -- function application

= State $ \s ->
  (\_ -> (s, ())) s
  -- definition of runState

= State $ \s -> (s, ())
  -- function application
```

And what's the implementation of `pure`?

```haskell
instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  (<*>) = ap
```

So we have arrived as expected at `pure ()`.

Now let's build a state monad as a free monad. To do so, we need to define a
functor that represents the operations `get` and `put`.

```haskell
data StateF s a
  = Get (s -> a)
  | Put s a
  deriving Functor
```

The type variable `a` is the type of the 'continuation', what to do after the
`Get` or the `Put`, and notice that in the case of `Get`, this continuation is
dependent on the state `s`. If we want to allow infinitely deep nesting of state
actions, we can take the fixed point `StateF s`.

```haskell
data Fix f = MkFix (f (Fix f))
```

This can be a bit hard to parse, so let's look more closely.
The type `f (Fix f)` takes the functor `f :: * -> *`, which we can imagine is
some kind of container, and fills all its slots with `Fix f`. This process is
then recursive! For example, the fixed point of `[]` (the list type
constructor), gives us infinitely nested lists, without actually being able to
put any values in the lists. This isn't particularly useful in practice, but it
does give us a way of representing the _shapes_ of finitely branching
trees. (Well, they could be infinitely branching because lists in Haskell can be
infinite, and again due to laziness, the trees could also be infinitely deep, too.)

Now back to state monads, what's `Fix (StateF s)`? It represents a list of
operations `Get` and `Put`, where after a `Get` we add to the context a variable
`x :: s` representing the state at that point. But unfortunately, there's no way
to end the list! And furthermore, the kind of `Fix (StateF s)` is just `*`, so
we can't write a `Monad` instance for this, killing our hopes of using `do`
notation.

Instead, we need a slightly different kind of fixed point, one where at each
step, we can choose to end the list. This data type is called `Free`.

```haskell
data Free f a
  = Pure a
  | Step (f (Free f a))
```

The `Step` constructor looks just like `MkFix` before, and the `Pure`
constructor lets us end the list with a value of type `a`.
Thanks to this extra type parameter, `Free f :: * -> *`, so we can write
`Functor` and `Monad` instances for this.
Indeed, if `f` is a `Functor`, then `Free f` is a `Functor` and even a `Monad`!
The idea of the `Functor` instance is to 'carry' the function `f` we're
`fmap`ping all the way to the value(s) of type `a`, using the underlying
functor `f`'s `fmap` at each step.

Although GHC can derive the `Functor` instance for us, I believe it's
informative to have a look anyway.

```haskell
instance Functor f => Functor (Free f) where
  fmap f = \case
    Pure x -> (f x)
    Step m -> fmap (fmap f) m
```

The `Monad` instance's `>>=` implementation follows a similar idea: traverse the
list to find the values of type `a`, and apply the given function
`a -> Free f b` to generate a new chunk of list to replace the `Pure`.

```haskell
instance Functor f => Monad (Free f) where
  f >>= k = case f of
    Pure x -> k x
    Step m -> (>>= k) <$> m

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

type StateFree s = Free (StateF s)
```

Now at last, we can use the `do` notation to build values of type
`Free (StateF s) a`. But it's inconvenient to use the actions `Get` and `Put`
directly, so let's define some helpers.

```haskell
get' :: StateFree s s
get' = Step (Get Pure)

put' :: s -> StateFree s ()
put' s = Step (Put s $ Pure ())
```

And now we can build a small program in our free monad.

```haskell
law2 :: StateFree s ()
law2 = get' >>= put'
```

We can ask ourselves finally, is `StateFree s` a state monad?
In other words, does `law2` work out to `pure ()`?

```haskell
get' >>= put'
= Step (Get Pure) >>= \s -> Step (Put s $ Pure ())
  -- by definition of get' and put'

= Step $ (>>= \s -> Step (Put s $ Pure ())) <$> Get Pure
  -- by definition of (>>=) for Free

= Step (Get $ (>>= \s -> Step (Put s $ Pure ())) . Pure)
  -- by definition of (<$>) for Free, Step case

= Step (Get $ \x -> (>>= \s -> Step (Put s $ Pure ())) (Pure x))
  -- by definition of (.)

= Step (Get $ \x -> Pure x >>= \s -> Step (Put s $ Pure ()))
  -- rewriting the operator section (>>= ...)

= Step (Get $ \x -> (\s -> Step (Put s $ Pure ())) x)
  -- by definition of (>>=) for Free, Pure case

= Step (Get $ \x -> Step (Put x $ Pure ()))
  -- by function application
```

And now there's no more evaluation we can do. So the answer to our question is a
firm _no_. `StateFree s` is not _necessarily_ a state monad.
What I mean by this 'necessarily' is that it ultimately depends on how
the `Get` and `Put` commands are _interpreted_.

Interpreting the free monad
---------------------------

We want to actually run the programs we write in our free monads, and to do so,
we need to convert this program into one operating in a monad that actually does
something. In our running example of state monads, we would like to interpret
the monad `StateFree s` into the monad `State s`.

Let's take a general approach. Our goal it to interpret a `Free f a` into a
target `m a`.
Surely, `Pure` and `Step` can be interpreted into `pure` and `>>=` in the target.
Then what's missing is a way to interpret the operations from the functor `f`.
So if we're given a general way to interpret a value `f a` into an `m a`,
then we should be able to interpret the whole `Free f a` into an `m a`.
That's the intuition behind the following implementation that I find
absolutely stunning.

```haskell
interp :: (Functor f, Monad m) => (forall a. f a -> m a) -> Free f a -> m a
interp phi = \case
  Pure x -> pure x
  Step m -> phi m >>= interp phi
```

It's the existence of this function that witnesses that `Free f` really is a
free monad, since it can thus be interpreted into _any_ monad `m`, provided a
way to interpret the functor `f` into the monad `m`.

Now let's put this to use, to implement the interpretation from `StateFree s` to
`State s`.

```haskell
interpState :: forall s a. StateFree s a -> State s a
interpState = interp phi where
  phi :: forall a. StateF s a -> State s a
  phi = \case
    Get f -> f <$> get
    Put s x -> put s *> pure x
```

What I find fascinating about this interpretation is both how safe and unsafe it
is at once. In the `Get` case, due to the `forall a` in the type of `phi`, we're
_forced_ to write the implementation as I wrote it. (Unless of course we use
exceptions or nontermination, but we never use those, do we?)
So the implementation of the `Get` case is perfectly safe.
On the other hand, in the `Put` case, there's nothing stopping us (except common
sense) from writing just `pure x`, forgetting to actually `put` the state!

Setting aside this safety issue for a moment, let's prove that this
interpretation is correct, in the sense that `interpState law2` works out to
`pure ()`

```haskell
interpState law2
= interpState $ Step (Get (\x -> Step (Put x $ Pure ())))
  -- by the derivation in the previous section

= interp phi $ Step (Get (\x -> Step (Put x $ Pure ()))) where
    phi = \case
      Get f -> f <$> get
      Put s x -> put s *> pure x
  -- by definition of interpState

= phi (Get (\x -> Step (Put x $ Pure ()))) >>= interp phi
    where phi = ...
  -- by definition of interp, Step case

= ((\x -> Step (Put x $ Pure ())) <$> get) >>= interp phi
    where phi = ...
  -- by definition of phi, Get case; I'll stop writing 'where phi = ...' each time.

= ((\x -> Step (Put x $ Pure ())) <$> State (\s -> (s, s))) >>= interp phi
  -- by definition of get from State

= State (\s -> (s, Step (Put s $ Pure ()))) >>= interp phi
  -- by definition of <$> for State, and function application

= State $ \s ->
  let (s', x) = (\s -> (s, Step (Put s $ Pure ()))) s in
  runState (interp phi x) s'
  -- definition of >>= for State

= State $ \s ->
  runState (interp phi (Step (Put s $ Pure ()))) s -- (1)
  -- function application, and let-elimination.

interp phi (Step (Put s $ Pure ()))
= phi (Put s $ Pure ()) >>= interp phi
  -- definition of interp, Step case

= put s *> pure (Pure ()) >>= interp phi
  -- definition of phi, Put case

= State (\_ -> (s, Pure ())) >>= interp phi
  -- definitions of *>, put, and pure.

= State $ \_ ->
  runState (interp phi (Pure ())) s
  -- definition of >>= for State

= State $ \_ -> (s, ()) -- (2)
  -- definition of interp (Pure case), and pure.

-- going back to (1)
(1)
= State $ \s ->
  runState (State $ \_ -> (s, ())) s
  -- by derivation (2)

= State $ \s -> (s, ())
  -- by runState and function application
```

And that's precisely `pure ()` as we hoped.

Law enforcement
---------------

As we saw in the last section, `StateFree s` is not a state monad. It ought to
be interpreted into a state monad, such as `State s`, but this interpretation is
not unique; we can write a bogus interpretation by ignoring the `Put` action.
In this section, we'll explore how we might avoid this problem.

Let's recall that a value of type `StateFree s` is essentially the syntax tree
of a program with two primitive operations `Get` and `Put`. These operations are
well-behaved if `Put`ting the result of a `Get` is a no-op and `Get`ting after
`Put`ting a value `s` is equivalent to just using the value `s`.

An approach to enforcing these laws would be to modify the syntax tree of
the program so that these 'redundant' pairs of operations are eliminated.
The result would be a program that necessarily satisfies the state monad laws,
because it would contain no places where the laws could apply.

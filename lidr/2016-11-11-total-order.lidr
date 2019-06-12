---
title: Decidable orderings in Idris
---

The goal of this post is to explore ordering relations in Idris. We will
discuss the notion of decision procedures, seeing how to decide equality of
natural numbers. We will define the notion of minimality of natural numbers and
we will show that minimality is a decidable property. Finally, we will
generalize the notion of minimality, and implement a verified lexicographic
total ordering for vectors.

This post is
[a literate Idris file](https://github.com/tsani/jerrington.me/blob/master/lidr/2016-11-11-total-order.lidr),
so here is the module declaration and necessary imports.

> module Data.Order.Total
>
> import Data.Vect
>
> %default total
> %access public export

## Decidable equality

In Idris, *decision procedures* are really useful. A decision procedure lets
you promote value-level information to the type-level, even at runtime. For
example, a decision procedure for equality produces a *decision* of the
equality of two things: either they are equal, in which case the procedure has
constructed a proof that they are equal; or they are not, in which case the
procedure has constructed a proof that the equality leads to a contradiction.
(In constructive logic, this is what is understood by a proposition being
false.)

We can represent decisions with a datatype.

```
data Dec : Type -> Type where
  Yes : prop -> Dec prop
  No : (prop -> Void) -> Dec prop
```

Let's implement a decision for the equality of natural numbers. Recall that
natural numbers are defined like this.

```
||| Natural numbers.
data Nat : Type where
  ||| Zero is a natural number.
  Z : Nat
  ||| Every natural number has a successor.
  S : Nat -> Nat
```

Obviously, zero is equal to zero, and zero is not equal to the successor of any
number, but what if we have two successors? Induction! Let's formalize this
argument with a function that computes the decision, i.e. a decision procedure
for the equality of `Nat`.

> decEqNat : (n : Nat) -> (m : Nat) -> Dec (n = m)

This reads "for all natural numbers $n$, and for all natural numbers $m$, we
can decide whether $n$ is equal to $m$".

> decEqNat Z Z = Yes Refl

Of course zero is equal to zero, and Idris recognizes this. The reason is
simple: when we match `Z` for `n` and `m`, *type refinement occurs*. This means
that in the type signature that is specialized for this branch of the function,
`n` and `m` are replaced by `Z` as well. Consequently, the goal of the function
has changed: we now have to produce `Dec (Z = Z)`. This is easy: we have `Refl
: x = x`, so unifying `x` with `Z` gives us a proof `Z = Z`. Then, wrapping
with `Yes` gives us a value of type `Dec (Z = Z)`, as required.

> decEqNat (S _) Z = No (\Refl impossible)
> decEqNat Z (S _) = No (\Refl impossible)

The syntax here is a bit tricky, but the idea is that the successor of a number
cannot be zero: because the constructors are different, Idris recognizes that
they are not equal. However, we need to show that them being equal is a
contradiction.

Suppose we wrote `No (\x => ?a)` and asked Idris the type of the hole, it would
say that `x : S _t = Z`. Matching with `Refl` gives the left-hand side type
error, because Idris cannot unify `S _` with `Z`. We explicitly mark the case
as impossible. But this is the only case
for the function! What happens when a function has no valid cases? The
intuition is that the function has been proven to be *uncallable*. When this
happens, the result type of the function is *arbitrary*. Because the function
can never be called, it may as well return whatever you like; it's not like you
could ever call it anyway. In particular, we can even produce values of type
`Void`, the type with no value, which is exactly what `No` requires.

The same reasoning applies to the symmetric case `decEqNat Z (S _) = ?a`.

At last, we have the inductive case. We are doing induction on both numbers at
once. If the two smaller numbers are equal, then it suffices to apply `S` to
both sides of the equality to show that the bigger numbers are equal. Equality
is a congruence, meaning that applying a function to both sides of an equality
produces another equality. This notion is captured by the function
`cong : (a = b) -> f a = f b`.

> decEqNat (S n) (S m) with (decEqNat n m)
>   | Yes eqPrf = Yes (cong eqPrf)

The `No` case is a bit trickier. Let's proceed the same way as in the previous
case to produce the contradiction.

 1. Writing `No (\x => ?a)` gives us a goal of `Void` with the assumptions
    `eqContra : (n = m) -> Void` and `x : S n = S m`.
 2. Matching `Refl` for `x` causes type refinement to occur: `S n` must unify
    with `S m`. However, Idris can make an additional inference. Since data
    constructors are *injective*, Idris infers also that `n` must unify with
    `m`, so the variable `m` is replaced by `n` in all our assumptions, giving
    us that `eqContra : (n = n) -> Void`.
 3. Of course, `Refl : n = n`, so we can apply `Refl` to `eqContra` and get
    `Void`, as required.

Using this reasoning gives the following equation.

>   | No eqContra = No (\Refl => eqContra Refl)

Now suppose we have a natural number coming from runtime - maybe we parsed a
number on standard input. We can check that the number is equal to, say, `3`
and get either a proof that this is so or that this is a contradiction. This
proof can then be used later in the program, helping us on the type level.

Equality is just one kind of relation. Can we generalize the theory we
developed for equality to other types of relations? The next simplest relation
to study is *orderings*.

## Decidable orderings

There are, very broadly speaking, two kinds of orders: partial orders and total
orders. In a partial order, it is possible to have two elements that are not
ordered, and we say that are *incomparable* in that case. In a total order, any
two elements are ordered in some way: either the first is "less than" the
second, or the second is "less than" the first. I use scare quotes because the
ordering can be other than numeric inequality, especially for partial orders.
(Also, the strict "less than" on numbers is not a total or partial order, but
it's shorter than writing "no greater than" all the time.)

For example, consider the *powerset boolean algebra* of the set
$A = \{a, b, c\}$. The elements in the boolean algebra are all possible
subsets of this set. The ordering relation is set containment: one set is
contained in an other (not to be confused with "is an element of another") if
all its elements are elements of the other set. So
$\{a, b\} \subseteq \{a, b, c\}$. However, neither
$\{a, b\} \subseteq \{b, c\}$ nor $\{b, c\} \subseteq \{a, b\}$, so this
ordering is partial.

To be a partial order, a few properties must be satisfied.
 1. The order must be reflexive: every element is "less than" itself
 2. The order must be transitive: if $A$ is "less than" $B$, and $B$ is "less
    than" $C$, then we can infer that $A$ is "less than" $C$.
 3. The order must be antisymmetric: if $A$ is "less than" $B$ and $B$ is "less
    than" $A$, then we can infer that $A = B$.
You can see for yourself that these properties are satisfied in the powerset
boolean algebra.

Total orders have all these same requirements, plus the additional requirement
that any two elements can be ordered. Intuitively, the natural numbers are
totally ordered by the less-than-or-equal relation.

Intuition isn't good enough though. Let's prove it. Here's the
less-than-or-equal relation for `Nat`. It is based on two ideas: zero is less
than any natural, and if we have that a natural is less than another, then we
can add one to "both sides".

```
data LTE : Nat -> Nat -> Type where
  LTEZero : LTE Z n
  LTESucc : LTE n m -> LTE (S n) (S m)
```

Now let's write a decision procedure for whether a given natural is less than
another one according this relation.

> decLte : (n : Nat) -> (m : Nat) -> Dec (LTE n m)

The zero case is easy, as usual. Since zero is less than any other natural, we
can use the `LTEZero` proof immediately.

> decLte Z _ = Yes LTEZero

On the other hand, if we have a successor in the first argument and a zero in
the second, we have a contradiction. How can a successor of some number be less
than or equal to zero? We just match both cases and dispel them as impossible,
which Idris confirms because the matches each produce a type error in the
left-hand side.

> decLte (S _) Z
>   = No (\x => case x of LTEZero impossible ; LTESucc _ impossible)

(Write down the unification constraints produced by the matches to see how the
contradiction arises in both cases.)

Finally, we have the inductive case, with successor in both arguments. We set
up the induction hypothesis with a recursive call in a with-clause, and then
see whether the smaller numbers are ordered or not. If they are, then we can
apply the `LTESucc` rule to infer that the larger ones are too.

> decLte (S n) (S m) with (decLte n m)
>   | Yes ltePrf = Yes (LTESucc ltePrf)

In the `No` case, we must prove that `LTE (S n) (S m) -> Void`. So we use a
lambda to introduce the assumption `LTE (S n) (S m)`. We can match this
assumption with `LTESucc p` to get `p : LTE n m`. The `No` match gives us a
proof that the smaller numbers being ordered is a contradiction, so we also
have the assumption `lteContra : LTE n m -> Void` available to us. It will
suffice to apply `p` to `lteContra` to get `Void`, as required.

>   | No lteContra = No (\(LTESucc p) => lteContra p)

With `decLte`, we can decide whether one number is less than another. However,
it turns out that this function cannot be used to decide which of two naturals
is *minimal*! Before continuing to explain why, let's see what exactly is meant
by "minimal".

> ||| A shorthand for inequality.
> (/=) : a -> b -> Type
> x /= y = Not (x = y)
>
> symNeq : (x /= y) -> (y /= x)
> symNeq neq eq = neq (sym eq)

> ||| Minimality among naturals.
> data Minimal : Nat -> Nat -> Type where
>   ||| The first natural is minimal: it is strictly smaller than the second.
>   MinLT : LTE n m -> (n /= m) -> Minimal n m
>   ||| Both are minimal: they are equal.
>   MinEQ : (n = m) -> Minimal n m
>   ||| The second natural is minimal: it is strictly smaller than the first.
>   MinGT : LTE m n -> (n /= m) -> Minimal n m

Now we would like to write a function to decide, given two numbers, which is
minimal. This is the *total order property* for `LTE`. Here's the signature of
our decision procedure.

> decMin : (n : Nat) -> (m : Nat) -> Minimal n m

It might seem surprising that `decLte` will be useless in implementing this
function. Using `decLte`, an implementation strategy would be to compute a
decision on whether the first argument is less than the second, and then
compute a decision on whether the second is less than the first. Then four
cases arise: `Yes/Yes`, `Yes/No`, `No/Yes`, and `No/No`. Intuitively, the last
case is a contradiction: how can neither number be minimal? Even so, we will be
unable to convince Idris that this case is in fact impossible. We would have in
that case the assumptions `notNM : LTE n m -> Void` and
`notMN : LTE m n -> Void`. Since we cannot match on function types, we cannot
use matching to tease the contradiction out of the data. The other option then
is to build either `LTE n m` or `LTE m n` so that we can invoke one our
assumptions to produce `Void`. (If ever you produce `Void` in a case, you can
dispel it with `absurd : Void -> a` which lets you derive anything.) However we
are *implementing* the function that determines either `LTE n m` or `LTE m n`
for any `n` and `m`; intuitively we *can't* build either such proof just like
that.

Instead, we will repeat more or less the same implementation as for `decLte`
and `decEqNat` and combine them.

> decMin Z Z = MinEQ Refl
> decMin Z (S _) = MinLT LTEZero (\Refl impossible)
> decMin (S _) Z = MinGT LTEZero (\Refl impossible)
> decMin (S n) (S m) = l (decMin n m) where
>   l : Minimal n m -> Minimal (S n) (S m)
>   l (MinLT lt neq) = MinLT (LTESucc lt) (\Refl => neq Refl)
>   l (MinEQ eq) = MinEQ (cong eq)
>   l (MinGT gt neq) = MinGT (LTESucc gt) (\Refl => neq Refl)

This formalism of minimality is quite powerful. Thanks to the embedded
in/equality proofs, we can use it to easily implement `decEqNat`

> decEqNat' : (n : Nat) -> (m : Nat) -> Dec (n = m)
> decEqNat' n m with (decMin n m)
>   | MinLT _ neq = No neq
>   | MinEQ eq = Yes eq
>   | MinGT _ neq = No neq

In fact, `decMin` is so strong that we can use it to implement `decLte` as
well, but we will need to use the fact that the `LTE` relation is
antisymmetric.

> lteAntiSym : LTE n m -> LTE m n -> n = m
> lteAntiSym LTEZero LTEZero = Refl
> lteAntiSym (LTESucc p) (LTESucc q) = rewrite lteAntiSym p q in Refl
>
> decLte' : (n : Nat) -> (m : Nat) -> Dec (LTE n m)
> decLte' n m with (decMin n m)
>   | MinLT lte _ = Yes lte
>   | MinEQ eq = case eq of Refl => Yes lteRefl
>   | MinGT gte neq = No (\lte => neq (lteAntiSym lte gte))

## Overloading total orders

### Generalizing minimality

We can generalize the proposition `Minimal` by refactoring the ordering
relation into a type parameter.

> data OrderedBy : {a : Type} -> (order : a -> a -> Type) -> (x : a) -> (y : a) -> Type where
>   LT : order x y -> (x /= y) -> OrderedBy order x y
>   EQ : (x = y) -> OrderedBy order x y
>   GT : order y x -> (x /= y) -> OrderedBy order x y

Of course, our `Minimal` is isomorphic to `OrderedBy LTE`.

> minimalityIsOrderedByLte : Minimal x y -> OrderedBy LTE x y
> minimalityIsOrderedByLte (MinLT lte neq) = LT lte neq
> minimalityIsOrderedByLte (MinEQ eq) = EQ eq
> minimalityIsOrderedByLte (MinGT gte neq) = GT gte neq
>
> orderedByLteIsMinimality : OrderedBy LTE x y -> Minimal x y
> orderedByLteIsMinimality (LT lte neq) = MinLT lte neq
> orderedByLteIsMinimality (EQ eq) = MinEQ eq
> orderedByLteIsMinimality (GT gte neq) = MinGT gte neq

I leave it as a (very boring) exercise to demonstrate that these functions are
inverses, and to tie this in with `Control.Isomorphism`.

### Overloading the axioms of total orders

We can collect all the axioms of a total ordering on some type into an
interface.

> interface TotalOrder (a : Type) (order : a -> a -> Type) | order where
>   constructor MkTotalOrder
>   orderRefl : order x x
>   orderTrans : order x y -> order y z -> order x z
>   orderAntiSym : order x y -> order y x -> x = y
>   orderTotal : (x : a) -> (y : a) -> OrderedBy order x y

Of course, our theory of natural numbers with `LTE` can be made into an
implementation of this interface.

> TotalOrder Nat LTE where
>   orderRefl = lteRefl
>   orderTrans = lteTransitive
>   orderAntiSym = lteAntiSym
>   orderTotal x y = minimalityIsOrderedByLte (decMin x y)

## Ordering vectors

Consider the words "cat" and "dog": the former comes before the latter, since
"c" comes before "d" in alphabetical order. What happens if the first letters
are equal?  We defer the ordering decision to the substring made by removing
the first letter. For example, consider "mouse" and "moose": since the first
two letters are the same, we compare "u" with "o" to conclude the "moose" comes
before "mouse".

The *lexicographic order* is a generalization of this way of applying
alphabetical order. We will show that any two vectors of the same length and
containing totally ordered elements (according to any total ordering) are
totally ordered by the lexicographic order. First, we need to define the
lexicographic order predicate for equal-length vectors.

> data Lexicographic : (sub : a -> a -> Type) -> Vect n a -> Vect n a -> Type where
>   LexEqZ : Lexicographic sub [] []
>   LexEqS
>     : (x = y)
>     -> Lexicographic sub xs ys
>     -> Lexicographic sub (x::xs) (y::ys)
>   LexLT : sub x y -> (x /= y) -> Lexicographic sub (x::xs) (y::ys)

The constructors `LexEqZ` and `LexEqS` are there to handle the fact that equal
vectors are lexicographically ordered. The `LexLT` constructor captures the
idea that if we find two unequal elements ordered by the underlying relation,
and those elements are the heads of arbitrary vectors, then those vectors are
lexicographically ordered.

It goes without saying that the proofs for `Lexicographic` are a bit trickier
than for `LTE`, so let's look at some examples.

> ||| The first elements are unequal and ordered by LTE, so the vectors are
> ||| lexicographically ordered.
> eg1 : Lexicographic LTE [1, 2, 3] [2, 1, 1]
> eg1 = LexLT (LTESucc LTEZero) (\Refl impossible)

> ||| The vectors are equal, so they are lexicographically ordered for any
> ||| choice of underlying order.
> eg2 : Lexicographic order ['c', 'a', 't'] ['c', 'a', 't']
> eg2 = LexEqS Refl (LexEqS Refl (LexEqS Refl (LexEqZ)))

> ||| The vectors are equal up to the second element, and are strictly ordered
> ||| in the third element.
> eg3 : Lexicographic LTE [1, 2, 3, 1] [1, 2, 5, 1]
> eg3 = LexEqS Refl (LexEqS Refl (LexLT (LTESucc (LTESucc (LTESucc (LTEZero)))) (\Refl impossible)))

Now let's show that lexicographic ordering over totally ordered elements is a
total order. To do so, we will show that it is reflexive, transitive,
antisymmetric, and totally decidable.

> lexRefl : {n : Nat} -> {v : Vect n a} -> Lexicographic o v v
> lexRefl {n=Z} {v=Nil} = LexEqZ
> lexRefl {n=S n} {v=x::xs} = LexEqS Refl lexRefl

Reflexivity is easy, since we built it into `Lexicographic` via the `LexEqZ`
and `LexEqS` constructors. Notice that reflexivity does not depend on any
properties of the underlying ordering on the elements in the vectors.

Transitivity is much harder. It will require the transitivity and antisymmetry
properties of the underlying ordering. Rather than require `OrderTotal` on the
underlying ordering right away, we can instead just take the transitive law and
antisymmetric law as arguments and use them when necessary.

> lexTrans'
>   : {a : Type}
>   -> {v1, v2, v3 : Vect n a}
>   -> {order : a -> a -> Type}
>   -> ({x, y, z : a} -> order x y -> order y z -> order x z)
>   -> ({x, y : a} -> order x y -> order y x -> x = y)
>   -> Lexicographic order v1 v2
>   -> Lexicographic order v2 v3
>   -> Lexicographic order v1 v3
> lexTrans' _ _ LexEqZ LexEqZ = LexEqZ

As usual, these base cases are very easy. Here, the matches cause type
refinement on the vectors making them empty. Since `v1 ~ []` and `v3 ~ []`, we
can prove that they're ordered with `LexEqZ`.

> lexTrans' _ _ (LexLT ord neq) (LexEqS Refl _) = LexLT ord neq
> lexTrans' _ _ (LexEqS Refl p) (LexLT ord neq) = LexLT ord neq

I'll focus on the first equation. The first pair of vectors have their first
elements ordered and unequal. The second pair of vectors have their first
elements identically the same. So the first element of the first vector is also
ordered with the first component of the third vector, and is also not equal to
it. That's enough to use `LexLT` to prove that the first vector is
lexicographically ordered with the third one, as required.

> lexTrans' trans antisym (LexEqS Refl p) (LexEqS Refl q)
>   = LexEqS Refl (lexTrans' trans antisym p q)

The first elements of the first *and* second pair of vectors in this case are
equal. So the first element of the first vector is equal to the first element
of the third. This is *almost* enough to use `LexEqS` to show the first vector
is lexicographically ordered with the third. We make a recursive call to prove
that the tails of the vectors are lexicographically ordered.

> lexTrans' trans antisym (LexLT {x} {y} ordl neql) (LexLT {x=y} {y=z} ordr neqr)
>   = LexLT (trans ordl ordr) (\Refl => case antisym ordl ordr of Refl => neql Refl)

Here is where we need to use the properties of the underlying ordering. What we
know is that the heads of the first pair of vectors are ordered, and the heads
of the second pair of vectors are ordered. Using the transitivity of the
underlying ordering, we can prove that the head of the first vector is ordered
with the head of the third vector.

Next, we need to prove that the head of the first vector is not equal to the
head of the third vector. Let `x`, `y`, and `z` refer to the heads of the
first, second, and third vectors respectively. Supposing that `x = z`, some
type refinement occurs, and we get that `ordl : order x y` and `ordr : y x`.
Using antisymmetry, we can conclude that `x = y`. Matching on that equality
proof unifies `x` with `y`, so all occurrences of `x` in types are replaced
with `y`. Therefore, `neql : (x = y) -> Void` becomes `neql : (y = y) -> Void`.
We can prove `y = y` with `Refl`, so we get produce `Void`, as required.

Now we can write a version of `lexTrans` where the properties on the underlying
ordering are obtained from `TotalOrder` instance.

> lexTrans
>   : (TotalOrder a order)
>   => {v1, v2, v3 : Vect n a}
>   -> Lexicographic order v1 v2
>   -> Lexicographic order v2 v3
>   -> Lexicographic order v1 v3
> lexTrans l1 l2 = lexTrans' orderTrans orderAntiSym l1 l2

For antisymmetry of lexicographic ordering, we will proceed in a similar way.
We'll write a general function that explicitly marks which properties of the
underlying ordering are needed. Then we can write an easy-to-use version that
fills in these functions by requiring a TotalOrder instance on the underlying
order.

The antisymmetry proof isn't very interesting, so I won't go into as much
detail as I did for transitivity. Due to how `Lexicographic` is designed, the
only way we could possibly have both `Lexicographic order v w` and
`Lexicographic order w v` at the same time is if both such proofs are `LexEqS`
chains. Consequently, the bulk of the antisymmetry proof is showing that
anything but chains of `LexEqS` are impossible, by deriving contradictions. The
key difference with earlier "contradiction fishing" is that now we will
generally construct `Void` on the right-hand side and dispel the case by using
the function `absurd : Void -> a`, which lets us derive anything.

> lexAntiSym'
>   : {v, w : Vect n a}
>   -> ({x, y : a} -> order x y -> order y x -> x = y)
>   -> Lexicographic order v w
>   -> Lexicographic order w v
>   -> v = w
> lexAntiSym' _ LexEqZ LexEqZ = Refl
> lexAntiSym' anti (LexEqS Refl p) (LexEqS Refl q) with (lexAntiSym' anti p q)
>   | Refl = Refl
> lexAntiSym' _ (LexLT ord neq) (LexEqS Refl q) = absurd (neq Refl)
> lexAntiSym' _ (LexEqS Refl p) (LexLT ord neq) = absurd (neq Refl)
> lexAntiSym' anti (LexLT ordl neql) (LexLT ordr neqr)
>   = absurd (neql (anti ordl ordr))
>
> lexAntiSym
>   : TotalOrder a order => {v, w : Vect n a}
>   -> Lexicographic order v w
>   -> Lexicographic order w v
>   -> v = w
> lexAntiSym = lexAntiSym' orderAntiSym

Last but not least, we need to show that lexicographic ordering is a decidable
ordering. Of course, in order to decide the lexicographic ordering of two
vectors, we will need to decide the ordering of individual elements according
to the underlying order; a function to do just that will be required as an
argument yet again, and in a revised version of the lexicographic ordering
decision procedure, this function will be supplied by a `TotalOrder` constraint
on the underlying ordering.

> decLex'
>   : {a : Type}
>   -> {order : a -> a -> Type}
>   -> ((x : a) -> (y : a) -> OrderedBy order x y)
>   -> (v : Vect n a)
>   -> (w : Vect n a)
>   -> OrderedBy (Lexicographic order) v w
> decLex' _ Nil Nil = EQ Refl
> decLex' decOrder (x::xs) (y::ys) with (decOrder x y)
>   | LT lt neq = LT (LexLT lt neq) (\Refl => neq Refl)
>   | GT gt neq = GT (LexLT gt (symNeq neq)) (\Refl => neq Refl)
>   | EQ eq with (decLex' decOrder xs ys)
>      | LT lt neq = LT (LexEqS eq lt) (\Refl => neq Refl)
>      | GT gt neq = GT (LexEqS (sym eq) gt) (\Refl => neq Refl)
>      | EQ eq' = case (eq, eq') of (Refl, Refl) => EQ Refl
>
> decLex
>   : TotalOrder a order
>   => (v : Vect n a)
>   -> (w : Vect n a)
>   -> OrderedBy (Lexicographic order) v w
> decLex = decLex' orderTotal

Finally, we can collect our four proofs into an instance of `TotalOrder` for
`Lexicographic order`, provided that `order` is also an instance of
`TotalOrder`.

> lexTotal : TotalOrder a order -> TotalOrder (Vect n a) (Lexicographic order)
> lexTotal _
>   = MkTotalOrder
>     (\x => lexRefl {v=x})
>     (\v1, v2, v3 => lexTrans {v1} {v2} {v3})
>     (\v, w => lexAntiSym {v} {w})
>     decLex

(I used the explicit form for defining a named implementation, because when
using the Haskell-style anonymous form, I would get a type error that made no
sense at all.)

## Conclusion

We saw how to implement some simple decision procedures on `Nat`, for equality
and for minimality according to `LTE`. We extended the notion of minimality to
describe general total orderings. We captured the notion of total orders in the
`TotalOrder` interface. Finally, we defined the lexicographic ordering for
vectors, and proved that it is a total order.

The takeaway from this post is not that lexicographic ordering is total or that
the minimal element of a set of two natural numbers can be decided
machanically. Instead, I hope that you were able to learn some new strategies
for proving things in Idris, and how powerful this language is.

There are a number of ways that the work here can be extended. The
`Lexicographic` datatype can be first generalized to work on lists. It can also
be generalized to work on vectors of possibly different lengths. Both such
lexicographic orders can be shown to be total.

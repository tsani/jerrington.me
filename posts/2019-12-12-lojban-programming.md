---
title: Programming in Lojban
---

Lojban would make for a cool dependently typed programming language.
I will refer to the programming language as {fancylojban}. This name is derived
from {fancu} and {lojban}, meaning "Function Lojban".

I will use the following quotation convention in this document.
- {curly braces} are for quoting proper Lojban text.
- `monospace` is for quoting Fancylojban code.
- "quotes" are for English text.

## Goals and anti-goals

I have three basic goals for this project.

* All programs should be syntactically correct Lojban texts.
* The text of a program should *suggest* what a program means under a usual
  interpretation of Lojban. One should not need to interpret things in a
  (too) non-standard or far-fetched way to understand a program.
* I want a small core language on top of which extensive syntax sugar can be
  defined to create a better user experience.

I have also some anti-goals, i.e. things that I am explicit *not* aiming for.

* I am not assigning a computational interpretation to arbitrary Lojban texts.
* I am not concerned with efficiency at all.

## Features of Fancylojban

* Pure: no observable side-effects.
* Total: all well-typed programs are terminating.
* Functional: functions are first-class. One can pass them to other functions
  and define functions that compute functions.
* Statically typed: Fancylojban has a static type system that rules out incorrect
  programs.
* Dependently typed: Fancylojban has dependent types, so type families depending on
  values can be defined. Fancylojban also supports large eliminations, so functions
  that calculate types can be defined.
* Typesafe: well-typed programs cannot crash the runtime.
* Executable: Fancylojban code has an operational semantics and can be executed.

## Types and values

A Fancylojban program consists of a number of definitions that make up a _signature_.
Each definition is a single sentence, delimited as usual by `.i`.

Certain kinds of definitions must be grouped, so the paragraph marker `ni'o` is
used to begin a definition block.

### Defining simple types

A type definition consists of two parts: the definition of the type constant
itself followed by zero or more constructor definitions.
Consisting of multiple definitions, a full type declaration must be placed in a
paragraph.
The first sentence of the paragraph must defined the type
constant.

```
ni'o si'o TYPE lo TYPE_1 lo TYPE_2 ...
```

The {si'o} abstractor defines a new type indexed by `TYPE_1`, `TYPE_2` and so
on. If no indices are provided, then a simple type is defined.

All types are defined by default within universe level zero. To define a large
type, at a higher universe level, one must explicitly state to what level the
type belongs.
The syntax concretely is to place `mu'ecmi lo PA moi` before the
definition and connect it to the definition with `gi'e`.

`mu'ecmi` is the lujvo formed from "munje" (universe) and "cmima" (set
membership).

```
ni'o mu'ecmi lo PA moi gi'e si'o TYPE ...
```

For example, I define a new type constant for natural numbers, at level zero.
```
.i si'o namcu
```

Now let's define constructors for this type.
Constructors are the canonical values of a type, so for natural numbers we have
two canonical values: zero is a natural number, and the successor of a natural
number is a natural number.

```
.i lo dzero cu namcu
.i lo sakcu be lo namcu cu namcu
```

These definitions can also be flipped for readability

```
.i namcu fa
   lo dzero
.i namcu fa
   lo sakcu be lo namcu
```

A constructor definition uses the type being constructed as the selbri. The
constructor itself is in the x1. Arguments to the constructor are given with
`be` (and `bei`).

### Polymorphism: types indexed by types

Fancylojban supports polymorphism by allowing quantification over a universe. This
enables the programmer to write generic code, e.g. generic lists.

```
ni'o si'o liste lo mu'ecmi
```

Here I use `lo mu'ecmi` to mean "a type", so I am indexing the definition of
`liste` with a type.

I define the empty list as a list of any type, and then define a cons cell by
requiring that the type of the element be the same as the type of the list.

```
.i ro da poi mu'ecmi zo'u
   lo nilso cu liste da
.i ro da poi mu'ecmi zo'u
   liste da fa
   lo konsi
      be lo me'au da
      bei lo liste be da
```

I use the special cmavo {me'au} to unbox the {mu'ecmi1} {da} to a type, which I
can then take the {lo} of to refer to an element of that type.

I use a universal quantifier `ro` in the above examples to quantify over all
types. `mu'ecmi2` is omitted, so it defaults to `lo no moi`, i.e. level zero.

I can construct a concrete list by assigning it a name with `goi`.

```
ni'o
  la .dzeros. goi
  lo konsi
  be lo dzero
  bei lo konsi
    be lo dzero
    bei lo konsi
      be lo dzero
      bei lo nilso
  cu liste lo namcu
```

This defines `la .dzeros.` in the below program as equal to the given concrete
list consisting of three zeroes.

Unlike in Lojban, in which {goi} is technically symmetric but in practice
defining the variable on the *right*, Fancylojban's {goi} operator requires the
variable begin defined to appear on the *left*.

### Polymorphism: types parametrized by types

A more convenient syntax exists for defining types parametrized by another type,
rather than indexed by another type.
See [this StackOverflow answer](https://stackoverflow.com/a/24601292/3722911)
regarding the difference.

Specifically, we can use the special selbri variables `brodV` (for any vowel V)
to refer to a type, and use tanru. This gives a very natural reading of {broda
liste} as "foo list".

```
ni'o si'o broda liste
.i lo nilso cu broda liste
.i lo konsi be lo broda bei lo broda liste
   cu broda liste
```

Here, the variable `broda` scopes over each definition in the block.
Each definition must construct a `broda liste`;
it wouldn't be possible to have one definition construct a `namcu liste`.
In contrast, with indices it is possible to have each constructor have a
different type (within the indexed family of types being defined), e.g.

```
ni'o si'o liste lo mu'ecmi (tozoigy. list indexed by a type gy.toi)
.i lo nacnilso cu liste lo namcu
.i lo nilso cu liste ro da poi mu'ecmi
.i ...
```

The `nacnilso` constructor constructs an empty list that is forces its index to
be `namcu`. Notice also in the definition of `nilso` here that we are not using
a prenex. As in ordinary Lojban, quantified term variables such as {da} can be
used outside a prenex. However they are subject to the following condition:

- If you use the variable more than once, the quantifier must only appear on one
  of the occurrences.
  
Since the quantified variable `da` is only appearing once, we can further
simplify the definition of `nilso` to

```
.i lo nilso cu liste ro mu'ecmi
```

which is syntax sugar for the `ro da poi ...` construct.

### Dependent types: types indexed by values

In the previous sections, I discussed types indexed by other types.
In Fancylojban, it is possible to index types by values. This defines a _family
of types_, with one proper type for each value of the index type.

For example, I can define the canonical type family: a list of numbers indexed
by its length.

```
ni'o si'o vekto lo namcu (tozoigy. "foo vector of <a number>" gy.toi)
.i lo veknilso cu vekto lo dzero
.i ro da poi namcu zo'u (tozoigy. for all numbers N ... gy.toi)
  vekto lo sakcu be da fa (tozoigy. a vector of (suc N) length is ... gy.toi)
    lo vekykonsi (tozoigy. the cons cell of ... gy.toi)
    be lo namcu (tozoigy. a number and ... gy.toi)
    bei lo vekto be da (tozoigy. a vector of length N gy.toi)
```

For example, we can build a concrete value of type `vekto li ci`. (I will allow
myself to write `li N` for any natural number N to mean `lo sakcu be ... lo
dzero` with `sakcu` appearing N times.)

```
ni'o la .vekcis. goi
  lo vekykonsi
    be li no
    bei lo vekykonsi
      be li pa
      bei lo vekykonsi
        be li re
        bei lo veknilso
  cu vekto li ci
```

We can mix indices and parameters to define a homogeneous vector of a
statically-known length.

```
ni'o si'o broda vekto lo namcu
.i lo veknilso cu broda vekto lo dzero
.i roda zo'u (tozoigy. can omit the type annotation on da ... gy.toi)
   broda vekto lo sakcu be da (tozoigy. because sakcu2 must be a namcu1 gy.toi)
   fa lo vekykonsi
     be lo broda
     bei lo vekto be da
```

The ordinary left-associativity of tanru give rise to a natural interpretation
of nested parameters. Here are some examples:

- `namcu liste vekto li N` is a "vector of length N of lists of numbers"
- `namcu vekto be li N vekto li M` is a
  "vector of length M of vectors of length N of numbers"

### Higher-order functions

The word `fancu` is used to form simple and dependent function types.
For example, the type of the function that calculates the length of a generic
list would be `fancu lo broda liste lo namcu`.
(Fancylojban does not use `fancu4`.)

Therefore we can express the type of the "map" function, which applies a function
to each element of a list to form a new list, as

```
fancu
  lo fancu be lo broda bei lo brode be'o
  ce'o
  lo broda liste
  lo brode liste
```

The word {ce'o} is used to form tuples. So I am expressing the type of a
function that

- takes as input a tuple of
  - a function from `broda` to `brode`
  - a `broda liste`
- and outputs
  - a `brode liste`
  
We will see shortly how to define this function, following the discussion of
recursion.

## Functions and computations

In this section, I will discuss how to define functions, both in a primitive
fashion and in a high-level fashion. Then, I will discuss how to perform
recursion on simple inductively defined datatypes such as `namcu`.

### Defining functions: the hard way

In the previous section, I constructed concrete lists and numbers by simply
using the appropriate type as the main selbri of a statement and giving the
value in the x1. I bound that value to a name using `goi`.
This same strategy can be used for defining a named function.
Suppose we wish to define a function from naturals to naturals that adds two to
its input. At a high level, this merely means applying the `sakcu` constructor
twice to the input.

```
ni'o
  lo ka relsumji ku goi
  ???
  cu fancu lo namcu lo namcu
```

Notice here that I have used {lo ka relsumji} on the left-hand side of `goi`.
This is called _pattern-matching {ka}_ since it defines the selbri variable
`relsumji`. For example, had we written `la relsumji ku goi ...`, this would
define the term `la relsumji` to be the function, thus requiring us to unbox `la
relsumji` to a selbri with {me'au} every time we actually want to call it.
Pattern-matching {ka} gives a lightweight way to define selbri resulting from
computations.

Now it suffices to construct an expression in `???` that has the appropriate
type. Such an expression ought to be an anonymous function, so I use Lojban's
{ka}-abstraction.
Each {ce'u} in the abstraction body denotes an input, and {makau} denotes the
output.

The way {ka}-abstractions are used in Fancylojban does differ from the way they
are used in Lojban. In Lojban, a {ka}-abstraction has a (strongly suggested)
implicit {ce'u} in the x1 position if no explicit {ce'u} is given. Furthermore,
multiple {makau} are allowed in Lojban. In Fancylojban, the rules are the
following.

- Exactly one {makau} must be present. If no {makau} is present, it is implied
  in the x1 position. It is an error for an x1 to be explicitly given if no
  {makau} is present.
- {ce'u} are never implicit. Each distinct occurrence of {ce'u} denotes a
  separate parameter.
- To reuse a parameter, there are two approaches:
  1. A name can be assigned to an occurrence of {ce'u} using {goi}, e.g.
     `ko'a goi ce'u`. The scope of {ko'a} is limited to the abstraction.
  2. A parameter prenex, ended by {ce'ai} can be given, e.g.
     `ka ko'a ko'e ce'ai broda`
     defines a function of two _named_ parameters `ko'a` and `ko'e` that can be
     used zero or more times in the function body.
- Unused parameters, for constructing functions that ignore an input, require
  the use of a {ce'ai} parameter prenex. You can avoid making up a name for an
  unused argument by using {zi'o}, e.g.
  `ka zi'o ko'a ce'ai broda`
  constructs a function of two parameters that ignores its first, unnamed
  parameter.
  
The complete example of defining a function that adds two to its input is therefore:
```
ni'o
  la relsumji ku goi
  lo ka sakcu lo sakcu be ce'u
  cu fancu lo namcu lo namcu
```

### Defining functions: the easy way

This is a quite verbose way to define a function. Fancylojban supports a
more lightweight, alternative syntax, in which the function selbri is defined
directly, involving the `ca'e` keyword.
The first sentence of the paragraph gives the type signature of the function
being defined. The second sentence gives its definition by using universally
quantified variables to refer to parameters.
Any variable is fine; it doesn't have to be from the {da}, {de}, {di}
series. You can use {ko'V} or {fo'V} series if you want, or even names with {la}.

```
ni'o ca'e lo namcu cu relsumji lo namcu
.i relsumji ro da
  fa lo sakcu be lo sakcu be da
```

This function has the following computational behaviour:

- `ma relsumji X` computes `lo sakcu be lo sakcu be X`, merely by following the
  definition of `relsumji`.

For example, I can define a higher-order function which accepts a function from
`broda` to `broda` and applies it twice to an input of type `broda`.

```
ni'o ca'e
  lo broda
  cu relfancu
  lo fancu be lo broda bei lo broda
  lo broda
.i relfancu ro da ro de
  fa lo me'au da be lo me'au da be de
```

Yuck. Since `da` has type `fancu lo broda lo broda`, I need to unbox it to a
selbri using {me'au} in order to apply it to an argument. Because I want to
apply the function twice, I need to do this unboxing twice.

To remedy this, we can use Fancylojban's _pattern-matching {ka}_ in order to
introduce selbri (higher-order) variables. Let's use this to try to define
`relfancu` again.

```
ni'o ca'e
  lo broda
  cu relfancu
  lo fancu be lo broda bei lo broda
  lo broda
.i relfancu lo ka bu'a ku ro da
fa lo bu'a
  be lo bu'a
    be da
```

This is great, since it avoids the use of the unboxing operator {me'au}.

Let's define another useful higher-order function, namely composition.

```
ni'o ca'e lo fancu be da be di
  fancyfancu
    lo fancu be de bei di
    lo fancu be da bei de
.i fancyfancu lo ka bu'a ku lo ka bu'e ku ro da
fa lo bu'a
  be lo bu'e
    be da
```

Using this, I can for instance redefine `relfancu` in terms of `fancyfancu`.

```
ni'o ca'e
  lo broda
  cu relfancu
  lo fancu be lo broda bei lo broda
  lo broda
.i relfancu ro da ro de
fa lo fancyfancu
  be da
  bei da
  bei de
```

Here we compose the input function with itself by calling `fancyfancu`. Note
that there was no need to use pattern-matching `ka` here since I just want to
pass the function along verbatim to `fancyfancu`.

Using `relfancu` and `relsumji`, I can define `vonsumji`, which adds four to
an input number. Here I demonstrate using pattern-matching {ka} with {goi} as
well as local definitions appearing in a prenex.

```
ni'o ca'e lo namcu cu vonsumji lo namcu
.i ro da
   lo ka bu'a ku
     goi lo ka relfancu me'ei relsumji ce'u
   zo'u
lo bu'a be da cu vonsumji da
```

First, note the use of `me'ei`. It is the inverse of `me'au`. Whereas `me'au`
converts a first-order term (a sumti) to a higher-order term (a selbri),
`me'ei` converts a higher-order term to a first-order term. I could have written
`lo ka relfancu` but this would have required additional terminators.

Second, note that `lo ka relfancu me'ei relsumji ce'u` is a _partial
application_ since `relfancu` is defined as taking two inputs.
This gives us that `bu'a` is equal to `relfancu me'ei relsumji` so `bu'a ko'a`
is `relfancu me'ei relsumji ko'a`.

Perhaps in this case however, it can be simpler to define the function using the
"hard" syntax and pattern-matching {ka}.

```
ni'o fancu lo namcu lo namcu
fa lo ka vonsumji ce'u ku goi
  lo ka relfancu me'ei relsumji ce'u
```

This is a true higher-order definition, since we simply defined `vonsumji` as a
partial application of `relfancu` to `relsumji`.

## Recursion

All the functions I have defined so far just shuffle parameters around and call
other functions. What if we want to do things with the datatypes we define? For
example, how can we define addition of numbers? The length of a list? The
concatenation of two vectors?

Such functions require the use of recursion.

An inductively defined type such as `namcu` admits an _induction principle_.
I will concentrate on recursion instead of induction here, since it's simpler.

Fancylojban has one general recursion operator `rekso` defined as
```
rekso = x1 is the result of recursion on x2 with cases x3 x4 ...
```

To specify which recursion principle is being used, I use a tanru: `namcu rekso`
refers to recursion on natural numbers.

For an inductive type such as `namcu`, there are two cases because there are two
constructors. For example, I define the `ckopi` function which copies a number
like this.

```
ni'o ca'e lo namcu cu ckopi lo namcu
.i ckopi ro da
fa lo namcu rekso
 be da
 bei lo dzero
 bei lo ka sakcu ce'u
cu ckopi ko'a
```

By the way, one could simplify `lo ka sakcu ce'u` to `me'ei sakcu`.

To understand why this copies a number, we need to understand the operational
semantics of `namcu rekso`.
The first case of `namcu rekso` is the case for zero, since we defined zero
first.
The second case is the case for successor, since we defined it second.

We have the following evaluation rules for `namcu rekso`:

1. `lo namcu rekso be lo dzero bei X bei Y` evaluates to `X`
2. `lo namcu rekso be lo sakcu be N be'o bei X bei Y` evaluates to
   `lo me'au Y be lo namcu rekso be N bei X bei Y`
  
where `me'au` is a function unboxing operator. That is, it converts a sumti
representing a function into a selbri.
Notice then that `Y` is receiving as its input the result of the recursive call
on the smaller number `N`!

In particular, it has the following evaluation rule

- `lo me'au me'ei broda ` evaluates to `lo broda`.
  (Recall that `me'ei broda` is `lo ka makau broda ce'u ... ce'u`.)

With these rules in mind, let's trace the evaluation of `lo ckopi be lo sakcu
be lo sakcu be lo dzero`, i.e. `copy 2`.

```
lo ckopi be lo sakcu be lo sakcu be lo dzero
=> lo namcu rekso
      be lo sakcu be lo sakcu be lo dzero
      bei lo dzero
      bei lo ka makau sakcu ce'u
=> lo me'au lo ka makau sakcu ce'u ku
      be lo namcu rekso
            be lo sakcu be lo dzero
            bei lo dzero
            bei lo ka makau sakcu ce'u
   (by namcu rekso rule 2)
=> lo sakcu be
      lo namcu rekso 
         be lo sakcu be lo dzero
         bei lo dzero
         bei lo ka makau sakcu ce'u
   (by me'au simplification rule)
=> lo sakcu be
      lo sakcu be
         lo namcu rekso
            be lo dzero
            bei lo dzero
            bei lo ka makau sakcu ce'u
   (by namcu rekso rule 2 and me'au simplification)
=> lo sakcu be
      lo sakcu be
         lo dzero
   (by namcu rekso rule 1)
```

So indeed `ckopi` does perform a recursive copy of its input.

Of course, this is a copying function that takes linear time to execute. What if
just want to define an identity function?

```
ni'o ca'e lo broda cu .aidzi lo broda
.i da .aidzi ro da
```

TODO: more examples of recursion: on lists, on vectors, etc.

## Induction

TODO: explain how to define predicates, and how to use induction to prove
properties about things.

## Pattern matching

TODO: explain pattern matching syntax as a generalization of the `ni'o ca'e`
construct.

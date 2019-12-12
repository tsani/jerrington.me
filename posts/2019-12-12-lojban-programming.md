---
title: Programming in Lojban
---

Lojban would make for a cool dependently typed programming language.

Defining a new type.
```
.i da'i ca'e TYPE lo TYPE_1 lo TYPE_2 ...
```
The {da'i ca'e} syntax defines a new type indexed by `TYPE_1`, `TYPE_2` and so
on. If no indices are provided, then a simple type is defined.

For example, I define a new type constant for natural numbers.
```
.i da'i ca'e namcu
```

Now let's define constructors for this type.
The general syntax for a constructor is

```
.i ca'e lo namcu cu namsero
.i ca'e lo namcu cu namsaku lo namcu
```

A constructor definition uses `lo TYPE` in the first position to indicate what
it is constructing. The arguments of the constructor go after.
In the above example, I define zero and successor respectively.

An inductively defined type such as `namcu` admits an induction principle. I
will concentrate on recursion instead of induction here, since it's simpler.

Lojban has one general recursion operator `rekso` defined as
```
rekso = x1 is the result of recursion on x2 with cases x3 x4 ...
```

To specify which recursion principle is being used, I use a tanru: `namcu rekso`
refers to recursion on natural numbers. It may be possible to infer this
information.

For an inductive type such as `namcu`, there are two cases because there are two
constructors. For example, I define the `ckopi` function which copies a number
like this.

```
.i
lo namcu rekso
   be ko'a
   bei lo namsero
   bei lo ka makau namsaku ce'u
cu ckopi ko'a
```

Here we are defining a function instead of a constructor, so we do not use
`ca'e`.

The function lists its input parameters after the name of the function, in this
case there is just one, `ko'a`.
Then I define how to calculate the output of the function by writing a term in
the first position of `ckopi`.

This syntax is a bit confusing because I give the name of the function and its
argument list *after* the body of the function. Luckily, Lojban supports
argument reordering operators. I allow for now only the use of {fa} to make
this one situation nicer.

```
.i ckopi ko'a
fa lo namcu rekso
      be ko'a
      bei lo namsero
      bei lo ka makau namsaku ce'u
```

To understand why this copies a number, we need to understand the operational
semantics of `namcu rekso`.
The first case of `namcu rekso` is the case for zero, since we defined zero
first.
The second case is the case for successor, since we defined it second.

We have the following evaluation rules for `namcu rekso`:
1. `lo namcu rekso be lo namsero bei X bei Y` evaluates to `Y`
2. `lo namcu rekso be lo namsaku be N be'o bei X bei Y` evaluates to
   `lo me'au Y be lo namcu rekso be N bei X bei Y`
  
where `me'au` is a function unboxing operator. That is, it converts a sumti
representing a function into a selbri.
In particular, it has the following evaluation rule

- `lo me'au lo ka makau broda` evaluates to `lo broda`.

With these rules in mind, let's trace the evaluation of `lo ckopi be lo namsaku
be lo namsaku be lo namsero`, i.e. `copy 2`.

```
lo ckopi be lo namsaku be lo namsaku be lo namsero
=> lo namcu rekso
      be lo namsaku be lo namsaku be lo namsero
      bei lo namsero
      bei lo ka makau namsaku ce'u
=> lo me'au lo ka makau namsaku ce'u ku
      be lo namcu rekso
            be lo namsaku be lo namsero
            bei lo namsero
            bei lo ka makau namsaku ce'u
   (by namcu rekso rule 2)
=> lo namsaku be
      lo namcu rekso 
         be lo namsaku be lo namsero
         bei lo namsero
         bei lo ka makau namsaku ce'u
   (by me'au simplification rule)
=> lo namsaku be
      lo namsaku be
         lo namcu rekso
            be lo namsero
            bei lo namsero
            bei lo ka makau namsaku ce'u
   (by namcu rekso rule 2 and me'au simplification)
=> lo namsaku be
      lo namsaku be
         lo namsero
   (by namcu rekso rule 1)
```

So indeed `ckopi` does perform a recursive copy of its input.

Of course, this is a copying function that takes linear time to execute. What if
just want to define an identity function?

```
.i ko'a .aidzi ko'a
```

There are still lots of questions to be answered:
- What exactly are the semantics of `be`?
- How do partial applications work?

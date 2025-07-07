---
title: Implementing dependent types. How hard could it be?
---

Short answer: not as hard as I thought.

Type systems in which the types (of functions, typically) may _depend_ on terms
are called _dependently-typed._ These systems exist on a spectrum, where restricted forms of
dependent types exist in common functional programming languages such as OCaml, Haskell, and even
TypeScript; the most complex forms of dependent types appear in proof assistants like Agda and Coq.

Dependent types complicate the typechecking process. Normally, a typechecker verifies, for
instance, that the type of the argument in a function application is compatible with the type of
the parameter of the function. In a simply-typed language, it suffices to check that the types are
equal. In the dependently-typed setting, types may contain terms requiring evaluation, so the
typechecker must _normalize_ types before proceeding to check equality. For example, the
typechecker must decide whether `3 + 5` (a term requiring evaluation) equals `8` in the
course of deciding whether a term of type `Vec (3 + 5) A` can be passed to a function expecting an
input of type `Vec 8 A`.

Let me be clear: _proving_ that every well-typed term in a dependently-typed language can be
normalized -- this is called the normalization property -- is very challenging. But that is not the
goal of this post.

In this post, I want to show you how we can _implement_ a normalization procedure using a powerful,
extensible technique called _normalization by evaluation_ (NbE) and subsequently implement a
typechecker for a small, dependently-typed language relying on this normalization procedure. I'll
begin, as do many existing presentations of these ideas, with a tiny language in which virtually
nothing of interest can be defined. I'll then scale up the language to include induction on natural
numbers and equality, enabling us to use this language to express simple proofs about arithmetic.
The typechecker will then act as a checker for these proofs. I'll comment on the (lack of)
ergonomics in the resulting system and conclude with why we say that implementing dependent types
is in fact challenging.

## The core calculus

If we allow terms to compute types and types to refer to terms, then the syntactic separation
between these objects disappears. Instead, I define a unified syntax that captures both, but
nonetheless use the letter `t` to refer to terms _understood as terms_ but the letters `A` and `B`
to refer to terms _understood as types._

```
Terms t, A, B ::= x | λx. t | t1 t2 | () | (x:A) -> B | ⊤ | ★
```

This syntax contains variables, lambda abstractions, applications, a constant `()` (unit), the
dependent function type `(x:A) -> B`, the unit type `⊤`, and the type of types `★`. (To simplify,
we'll use the rule `★ : ★`.)

With this syntax, we can write things we couldn't in the simply-typed lambda calculus. For example,
consider the type `(x:⊤) -> (λx.⊤) x`. From the point of view of the simply-typed lambda calculus,
this type is very unusual: it contains a reducible expression (redex), namely the right-hand
side of the arrow can be simplified to just `⊤` by beta-reduction.

This syntax also allows for quantification over types, allowing us to write polymorphic functions,
too. For example, here's the type of a (simply-typed) composition operator.

    (A:★) -> (B:★) -> (C:★) -> (f : B -> C) -> (g : A -> B) -> (x:A) -> C

We encode the syntax of the core calculus as a recursive type.

```ocaml
type ix = int

type tm =
    (* terms *)
    | Lam of name * tm
    | Var of ix
    | App of tm * tm
    | Unit
    (* types *)
    | Pi of (name * tp) * tp
    | Top
    | Star
and tp = tm (* to understand a term as a type *)
and ctx = (name * tp) list
```

This syntax uses a nameless (de Bruijn) representation for variables in order to simplify the
implementation of capture-avoiding substitution, but I do store names of variables in binders and
contexts as a compromise to enable writing a pretty-printer that shows names.

Our goal is to implement a typechecker for this syntax: a procedure on pairs of terms together with
a context to decide the typing relation. That relation is defined inductively by a natural
deduction-style set of inference rules, which I'll give later. For now, suffice to say that in
these rules will appear some substitutions like `[t/x]A`. Substitution does not preserve normal
forms, so a critical piece of infrastructure in building our typechecker is a normalization
procedure.

## Normalization by Evaluation

A term is in normal form when it cannot be further reduced. Suppose `t` and `A` are in normal form,
and that `A` contains a free variable `x`. Will `[t/x]A` be in normal form?

Not necessarily! That's the central challenge in implementing our typechecker, which we will
surmount by implementing a normalization procedure.

Normalization by Evaluation is a semantic approach to normalization. It is made up of two related
procedures.

1. `eval` is a bog-standard evaluation procedure, which interprets a lambda-term into a semantic
   space of values. This procedure is responsible for finding the beta-normal form of a lambda-term
   by eliminating all redices from the term.
2. `quote` is its inverse, which reifies an element from the semantics (a value) back into a
   lambda-term. This procedure is responsible for eta-expanding terms at function types, enabling
   us to consider `f` and `λx. f x` as judgmentally equal at the type `A -> B`.

The game plan to implement this pair of procedures is first to define the semantic space, i.e. the
set values; then to implement `eval`; and finally to implement `quote`.

### Values

The value of a term is its semantics. Since we're working in OCaml, we can give the semantics of a
lambda-term at function type as an OCaml function. In other words, we design our evaluation
procedure so that if `t : A -> B`, then `eval(t)` gives us an OCaml function from values of type
`A` to values of type `B`. This is a good idea, operationally speaking, because we get to wait
until the argument to the function is known (and evaluated) before proceeding to evaluate the
function body. The same line of reasoning leads to interpreting a Pi-type as an OCaml function on
types.

```ocaml
and value =
    | VLam of name * (value -> value)
    | VUnit
    | VPi of (name * vtp) * (value -> vtp)
    | VTop
    | VStar
```

This syntax of values is good enough for closed terms, having no free variables, but it is
insufficient for open terms. Concretely, we will run into a problem when trying to implement
`quote` later on: how will we reconstruct a lambda-term from an OCaml function `value -> value`?
We want to quote the body of the abstraction -- that is the output of the function `value -> value`
-- but to access the body, we must apply this OCaml function to some value. What value can we use?

The resolution to this quandry lies in generalizing the syntax of values to explicitly account for
open terms, while syntactically restricting the representation to allow only beta-normal forms: it
isn't enough to just add a constructor for variables; once we introduce variables to the syntax,
then e.g. `x (λx. y)`, an application in which a variable appears in the _head position,_ is also
in beta-normal form.

Rather than merely add variables, we add a separate syntax of so-called _neutral terms._ These are
variables and elimination forms applied to neutral terms. Conceptually, a neutral term is a stack
of elimination forms that are ultimately blocked on a variable. In contrast, the syntax of values
I gave above corresponds to the introduction forms of the lambda-calculus.

```ocaml
and value =
    | VLam of name * (value -> value)
    | VUnit
    | VPi of vtp * (name * (value -> vtp))
    | VTop
    | VStar
    | VN of neu (* neutral terms embed as values *)

and neu =
    | NVar of lvl
    | NApp of neu * value

and lvl = int
```

Notice that the representation I use for variables here mentions `lvl` -- these are de Bruijn
levels, the dual of de Bruijn indices. When variables use de Bruijn levels, weakening such terms
becomes free, whereas the use of de Bruijn indices would require costly explicit shifts. This will
end up simplifying the implementation of `eval`.

Finally, this combined syntax of values and neutral terms is specifically designed to represent
only beta-normal forms. A value is a stack of introduction forms that might some point "switch" to
a neutral term, which is then a stack of elimination forms ending on a variable. Crucially
impossible is to write an elimination form whose subject is an introduction form, i.e. a
non-beta-normal term.

### `eval`

Recall from kindergarten how to evaluate lambda-terms, and use your imagination to extend the
procedure to types. We'll use an environment-based approach to efficiently handle substitutions in
a lazy fashion. In the presence of neutral terms, whenever we evaluate an elimination form, we need
to explicitly check whether the subject of the elimination is normal or neutral to proceed
accordingly.

* **Elimination subject is normal.** A reduction is therefore possible, so we must perform it.
* **Elimination subject is neutral.** Reduction is ultimately blocked on a variable, so we extend
  the stack of blocked eliminations.

```ocaml
and env = value list

let rec eval (e : env) (t : tm) : value =
    match t with
    (* types: *)
    | Top -> VTop
    | Star -> VStar
    | Pi ((x, tA), tB) -> VPi (eval e tA, (x, fun v -> eval (v::e) tB))
    (* terms: *)
    | Unit -> VUnit
    | Lam (x, t) -> VLam (x, fun v -> eval (v::e) t)
    | Var i -> List.nth e i
    | App (t1, t2) -> apply (eval e t1) (eval e t2)

and apply v1 v2 = match v1 with
    | VN n -> VN (NApp (n, v2))
    | VLam (_, f) -> f v2
    | _ -> failwith "type error"
```

<aside>
Notice the anonymous function used in both the `Pi` and `Lam` cases is the same. We could easily
rewrite `eval` (and `value`) in a
[defunctionalized](/posts/2023-02-12-defunctionalizing-continuations) form -- that is, we could use
a first-order representation of closures instead of using OCaml's closures -- leading to a strategy
suitable for efficient implementation in a lower-level language.
</aside>

### `quote`

Equipped with `eval` to perform reductions, interpreting a lambda-term into an OCaml semantics,
we're now ready to implement its dual, to finally arrive at a normalization procedure by composing
the two. This dual procedure, called `quote`, transforms a semantic value back into syntax.

What's conceptually tricky about `quote` is how we handle functions. Recall that `Lam (x, t)`
evaluates to the (metalanguage) closure `fun v -> eval (v::e) t` where the (meta) free variable `e`
is an environment providing values for the (object) free variables present in `t`. To quote this,
we must apply the closure to a value. Thankfully, we have neutral terms at our disposal: we
generate a variable, corresponding to the bound variable of the abstraction, to use as an argument.

Since neutral variables use de Bruijn levels, the `quote` procedure tracks a current depth,
incremented by one whenever it traverses a binder.

It is `quote` that's responsible for finding an eta-normal form, by performing eta-expansion of
neutral terms at function types. This requires typing information, which quote will ultimately
receive from the typechecker in the form of a semantic type. Then, `quote` must itself essentially
do the job of bidirectional typechecking by traversing the given value together with its (semantic)
type in order to maintain typing information at every step of the quoting process.

To know the type of a variable when quoting encounters it, we equip the process with a map from
variables to semantic types. This mapping is called a _typing environment,_ in contrast with an
(ordinary) context that maps variables to _syntactic_ types and an (ordinary) environment that maps
variables to values (semantic terms). Since we conflate types and terms in our setup, a typing
environment is really nothing more than an ordinary environment.

```ocaml
type tp_env = env
```

Finally, since `quote` captures the checking mode of bidirectional typechecking as it traverses a
value (normal term) together with its semantic type, we pair it with a separate `quote_neu` to
traverse a neutral term, synthesizing a type from it and the typing environment. (The following
section will make more precise the idea of bidirectional typechecking.)

```ocaml
let vvar l = VN (NVar l)

let rec quote (d : lvl) (e : tp_env) (vA : vtp) (v : value) : tm =
    match vA, v with
    (* types: *)
    | VStar, VTop -> Top
    | VStar, VStar -> Star
    | VStar, VPi (vA, (x, fB))->
        Pi ((x, quote d e Star vA), quote (d+1) ((x, vA)::e) Star (fB (vvar d)))
    (* terms: *)
    | VTop, VUnit -> Unit
    | VPi (vA, (x, fB)), VLam (y, f) ->
        Lam (x, quote (d+1) (fB (vvar d)) (f (vvar d)))
    | VPi (vA, (x, fB)), VN n -> (* eta-expansion of neutral terms at function type *)
        Lam (x, quote (d+1) (fB (vvar d)) (VN (NApp (n, vvar d))))
    | _, VN n -> quote_neu d e n |> fst
    | _ -> failwith "ill-typed"

and quote_neu (d : lvl) (e : tp_env) : neu -> tm * vtp = function
    | NVar l ->
        let i = lvl2ix d l in
        let vA = ix_lookup i e in
        (Var i, vA)
    | NApp (n, v) ->
        let t, vAB = quote_neu d e n in
        begin match vAB with
        | VPi (vA, (x, fB)) ->
            (App (t, quote d e vA v), fB v)
        | _ -> failwith "ill-typed"
        end
```

### Normalization

A roundtrip through eval and quote brings a term into normal form.

```ocaml
let norm (t : tm) (tA : tp) : tm =
    quote 0 [] (eval [] tA) (eval [] t)
```

## Bidirectional typechecking

Bidirectional typechecking is a technique that exploits a duality within terms to reduce the amount
of typing information required from the programmer. We divide the syntax of terms into _normal
terms_ and _neutral terms,_ precisely as we did for values, giving a syntactic characterization of
beta-normal terms.

* **Normal terms** are associated with _constructors_ and are those terms whose typechecking is
  also type-directed. In other words, we _check_ that a given normal term has a _given_ type.
* **Neutral terms** are associated with _eliminators_ and are those terms from which their type may
  be _synthesized._ In other words, we _infer_ the type of a neutral term in a given context.

For example, in order to appropriately extend the context during typechecking, we classify a lambda
abstraction as a normal term. Hence, its type is given, making the context extension
straightforward to compute. In picking apart the given type of the lambda abstraction, we obtain
the expected type of the abstraction's body, so we equally classify the abstraction body as normal.

Dually, we expect a function application to be neutral. To infer the type of an application, we
first infer the type of its subject -- requiring that the function be neutral assures
beta-normality -- and insist that the type be a Pi-type `(x:A) -> B`. This in turn reveals the
expected type `A` of the function's argument, so we let the argument be normal.

Here's a BNF grammar.

```
Normal terms  t, A, B  ::= λx.t | () | (x:A) -> B | ⊤ | ★ | s
Neutral terms       s  ::= x | s t
```

This exposition shows intuitively that typechecking beta-normal terms is some way easier. We begin
by typechecking a normal term against its given type. The term, being a stack of introduction
forms, is structurally aligned with its type. In doing so, we can easily extend the context at
binding sites. Typechecking switches modes into type inference, when the normal term switches to a
neutral term. Information stored in the context comes into play upon encountering a variable, and
furthermore when inferring the type of the subject of a function application.

Although we could encode normal and neutral terms as two separate OCaml types, as we did our
semantic domain, I won't bother. Instead, typechecking will raise an exception if it encounters
such situations.

Crucially, since this calculus is dependently-typed,
substitutions -- `[t/x]A` denotes the capture-avoiding substitution of all free occurrences of `x`
in `A` with `t` -- will appear in these typing rules.

I'll give only rules that differ from the those of the simply-typed lambda calculus.

```
    G, x:A |- t : B              G(x) = A
---------------------------    ------------
  G |- λx.t : (x:A) -> B        G |- x : A


  G |- t : (x:A) -> B    G |- t' : A
--------------------------------------
         G |- t t' : [t'/x]B


  G |- A : ★    G, x:A |- B : ★
---------------------------------
       G |- (x:A) -> B : ★

--------------      --------------
  G |- ★ : ★          G |- ⊤ : ★
```

Yes, that's the type-in-type rule, meaning that the resulting system is unsound. I'll explore later
how we might introduce universes to address that.

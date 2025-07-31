---
title: 'Implementing dependent types: how hard could it be? (Part 2)'
---

Short answer: hard, but not as hard as I thought.

This is part 2 of a series of posts on the implementation of a dependently-typed lambda-calculus.
In part 1, we discussed the need for normalization of terms, and used a powerful, extensible
approach called Normalization by Evaluation to implement a normalization procedure.
In this post, we'll implement the typechecking procedure for our small calculus, and crucially use
the normalization procedure to do.

## Bidirectional typechecking

Bidirectional typechecking is a technique that exploits a duality within terms to reduce the amount
of typing information required from the programmer. We divide the syntax of terms into _normal
terms_ and _neutral terms,_ precisely as we did for values, giving a syntactic characterization of
beta-normal terms.

* **Normal terms** are associated with _constructors_ and are those terms whose typechecking is
  also type-directed. In other words, we _check_ that a given normal term has a _given_ type.
  Normal terms are also sometimes called _checkable terms._
* **Neutral terms** are associated with _eliminators_ and are those terms from which their type may
  be _synthesized._ In other words, we _infer_ the type of a neutral term in a given context.
  Neutral terms are therefore sometimes called _synthesizable_ or _inferrable_ terms.

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

This exposition shows intuitively that typechecking beta-normal terms is in some way easier. We
begin by typechecking a normal term against its given type. The term, being a stack of introduction
forms, is structurally aligned with its type. In doing so, we can easily extend the context at
binding sites. Typechecking switches modes into type inference, when the normal term switches to a
neutral term. Information stored in the context comes into play upon encountering a variable, and
furthermore when inferring the type of the subject of a function application.

We express this bimodal typechecking scheme formally using a pair of mutually defined judgments.

* `G |- t <= A` normal term `t` checks against given type `A` in context `G`.
* `G |- s => A` neutral term `s` synthesizes type `A` in context `G`.

These judgments are defined inductively by the following rules. Let's start with some
straightforward ones.

```
    G, x:A |- t <= B              G(x) = A
---------------------------    ------------
  G |- λx.t <= (x:A) -> B        G |- x => A


  G |- t => (x:A) -> B    G |- t' <= A
--------------------------------------    --------------
         G |- t t' => [t'/x]B              G |- () <= ⊤

  G |- A <= ★    G, x:A |- B <= ★
---------------------------------     --------------
       G |- (x:A) -> B <= ★              G |- ⊤ <= ★


--------------
  G |- ★ <= ★
```

Yes, at the bottom there is the type-in-type rule, meaning that the resulting system is unsound.
I'll explore later how we might introduce universes to address that.

```
 G |- s => B    G |- A ≡ B : ★
-------------------------------
         G |- s <= A
```

This rule captures the mode-switching, from checking to inference. When we need to check a
synthesizable term against a given type, we synthesize the type of that term before checking that
the expected and actual type are equal as types `G |- A ≡ B : ★`. In practice, we'll already have
`A` and `B` be in normal form, so the equality check is merely syntactic!




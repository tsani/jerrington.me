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

This rule captures mode-switching, from checking to inference. When we need to check a
synthesizable term against a given type, we synthesize the type of that term before checking that
the expected and actual type are equal as types `G |- A ≡ B : ★`. In practice, we'll already have
`A` and `B` be in normal form, so the equality check is merely syntactic!

Now it suffices to encode these rules into a pair of functions in OCaml. The hard case will be
function application, as there's a substitution there and we don't yet know how to compute those.

```ocaml
let rec check (cG : ctx) (t : tm) (tA : tp) : unit =
    match tA, t with
    | Star, Top -> true
    | Star, Star -> true
    | Star, Pi ((x, tA), tB) ->
        check cG tA Star;
        check ((x, tA)::cG) tB Star
    | Top, Unit -> true
    | Pi ((_x, tA), tB), Lam (x, t) ->
        check ((x, tA)::cG) t tB
    | tA, s ->
        let tA' = synth cG s in
        (* and now we need to check that tA' = tA *)

and synth (cG : ctx) (t : tm) : tp =
    match t with
    | Var i -> List.nth cG i |> snd
    | App (s, t) ->
        begin match synth cG s with
        | Pi ((x, tA), tB) ->
            check cG t tA;
            (* and now we need to compute and return [t/x]tB *)
        end
```

In the code above, there are two TODOs to resolve. The first one is easy. We'll insist that before
the initial call to `check`, we have already normalized the given type `tA`. Then, we'll need to
insist that `synth` also return a type in normal form. Hence we resolve the first TODO by
performing a syntactic equality check on `tA` and `tA'`. If we had used a completely nameless
representation, it would suffice to use OCaml's built-in equality `tA' = tA`, but on account of our
compromise of keeping some names, we'll actually need to implement a `tm_eq` function that ignores
names. I'll omit the code of that function.

The second TODO is more sensitive. Even if we assume that `t` and `tB` are in normal form, the
result of the substitution `[t/x]tB` might not be.

## Substitution

As a first approach, we might implement a function `subst : (tm * ix) -> tm -> tm` such that `subst
(t, x) tB` computes the substitution `[t/x]tB`, then use the NbE procedure from part 1 of the
series to normalize.

This first approach sucks for two reasons. First, implementing substitution on nameless terms
requires implementing various shifting (weakening) helpers to account for those situations where
the substitution crosses a binder such as a lambda abstraction. Second, we end up effectively
traversing the term `tB` _three times_: once for the substitution, then twice during normalization
(once for `eval`, and once for `quote`).

Funny enough, we already implemented a clever substitution procedure that addresses both those
issues! Remember `eval`? It replaces all free variables in a term with the values held in an
environment. The values in the environment crucially represent variables using de Bruijn _levels,_
not indices, which gives us weakening for free. No need for any shifting.

It suffices to construct an appropriate environment for the substitution we wish to perform, use
`eval` to perform it, and use `quote` to convert back to syntax. We therefore avoid the tricky
business of implementing nameless substitutions entirely, and cut down on the number of term
traversals required.

To build the environment we need, consider that `t` lives in context `cG` and `tB` lives in context
`(x, tA)::cG` -- those contexts tell us how many free variables appear in these terms. We need to
convert the context `cG` into a 'dummy environment' `eG` that maps each variable back to itself.
Then, we can evaluate `t` in the environment `eG` to get a value `v` to form the
environment `(x, v)::eG` to finally evaluate `tB`. This will replace the variable `x` with `v`, as
required!

To convert a context into such a 'dummy environment', let's implement `ctx2env`.

```ocaml
let vvar l = VN (NVar l)

let rec ctx2env (cG : ctx) : env * int = match cG with
    | [] -> ([], 0)
    | (x, _)::cG ->
        let eG, n = ctx2env cG in
        ((x, vvar n)::eG, n+1)
```

In order to 'count backwards' to compute the correct levels, `ctx2env` also computes as a
by-product the length of the context.

Next, let's fill in the second TODO of the typechecker, following the gameplan I outlined above.

```ocaml
let rec check (cG : ctx) (t : tm) (tA : tp) : unit = ...
and synth (cG : ctx) (t : tm) : tp =
    match t with
    | Var i -> List.nth cG i
    | App (s, t) ->
        begin match synth cG s with
        | Pi ((x, tA), tB) ->
            check cG t tA;
            let eG, n = ctx2env cG in
            let v = eval eG t in
            let vB = eval ((x, v)::eG) tB in
            (* TODO: `quote vB` *)
        end
```

Quoting `vB` is a bit tricky. Recall from part 1 that quoting is type-directed, so we need to
supply the type at which we're quoting, and a typing environment that maps each free variable to
its semantic type. We needed this typing information in order to perform correct eta-expansion.

The type at which we're quoting is easy. Since we're quoting a type, we're quoting _at type_
`VStar`. The typing environment, on the other hand, is harder to come by. We need to evaluate each
of the types in `cG`. But then each entry in cG lives in the subcontext of remaining entries. Yuck.
Let's implement a helper `ctx2tyenv` that can at least leverage the `eG` we already computed.

```ocaml
let rec ctx2tyenv (cG : ctx) (eG : env) : tp_env =
    match cG, eG with
    | [], [] -> []
    | (x, tA)::cG, _::eG ->
        (* cG |- tA : ★
           and eG is the dummy env of cG *)
        let vA = eval eG tA in
        (x, vA) :: ctx2tyenv cG eG
```

Finally we can quote `vB`.

```ocaml
let rec check (cG : ctx) (t : tm) (tA : tp) : unit = ...
and synth (cG : ctx) (t : tm) : tp =
    match t with
    | Var i -> List.nth cG i
    | App (s, t) ->
        begin match synth cG s with
        | Pi ((x, tA), tB) ->
            check cG t tA;
            let eG, n = ctx2env cG in
            let v = eval eG t in
            let vB = eval ((x, v)::eG) tB in
            quote n (ctx2tyenv cG eG) VStar vB
        | _ -> failwith "ill-typed: application subject not a function"
        end
```

Unfortunately, this approach merely trades one inefficiency for another.

Stop and consider for a moment that applications are often nested. This means what the subterm `s`
is likely to be yet another application. We end up repeatedly calling `ctx2env` and `ctx2tyenv` on
the same context `cG`, meaning we repeatedly evaluate the types in that context.

Moreover, in the event of nested applications, notice that we quote `vB` only to get back another
Pi-type on whose subterms we would then redundantly `eval` again!

## Leaning on the semantics

Rather than repeatedly convert back and forth from syntax into semantics, we could instead work
more closely with the semantics. For instance, recall that we imposed a precondition on `check`,
that the given type would be in normal form. In that case, let's not take the type as a syntactic
`tp`, but rather as a semantic `vtp`, which we designed to capture only beta-normal forms anyway.
Likewise, the context ought to only store types in normal form, too, so why not just use a `tp_env`
instead of a `ctx`? And finally, if `synth` should output a type in normal form, again let's output
a `vtp` instead.

Crucially, the only one that remains as a raw, syntactic term is the term under consideration for
checking or synthesis. What's more, that's the only term we **can't** evaluate first, as we don't
yet know whether it's well-typed!

Besides representing everything that ought to be in normal form as a value, we'll also want to
avoiding using `ctx2env` to generate the 'dummy' environment we need when calling `eval` during
typechecking. To do so, the typechecker will also track a dummy environment. Whenever it goes under
a binder, we will extend not only the typing environment but also the dummy environment. To extend
the dummy environment, we need to generate a new variable whose level is the current length of the
dummy environment. To avoid computing this length, we'll also track it as we go.

```ocaml
let rec check (d : lvl) (e : env) (eG : tp_env) (t : tm) (vA : vtp) : unit =
    match vA, t with
    | VStar, Top -> ()
    | VStar, Star -> ()
    | VStar, Pi ((x, tA), tB) ->
        check d e eG tA VStar;
        let vA = eval e tA in
        check (d+1) ((x, vvar d)::e) ((x, vA)::eG) tB VStar
    | VTop, Unit -> ()
    | VPi ((_, vA), fB), Lam (x, t) ->
        check (d+1) ((x, vvar d)::e) ((x, vA)::eG) t (fB (vvar d))
    | vA, s ->
        let vA' = synth d eG s in
        let tA = quote d eG VStar vA in
        let tA' = quote d eG VStar vA' in
        if not (tm_eq tA tA') then failwith "type mismatch"

and synth (d : lvl) (e : env) (eG : tp_env) (t : tm) : vtp =
    match t with
    | Var i -> List.nth eG i |> snd
    | App (s, t) ->
        begin match synth d e eG s with
        | VPi ((x, vA), fB) ->
            check d e eG t vA;
            let v = eval e t in
            fB v
        | _ -> failwith "ill-typed: application subject not a function"
        end
    | _ -> failwith "cannot synthesize type of checkable term"
```

There are two things that remain a bit unpleasant about this implementation. In the last case of
`check`, we need to check that `vA` equals `vA'`, and we do so by quoting both and comparing as
terms. It's a bit wasteful to _fully_ quote both values if it turns out they aren't equal though.
When they _are_ equal, we still end up performing three traversals: once for each quote, and once
more for `tm_eq`.

We can do better by implementing an equality procedure directly on values. Essentially, the
procedure does the job of quoting (and hence eta-expanding) both values as long as they match,
stopping early as soon as it detects that they don't.

Since we need to maintain typing information along the way (to support eta-expansion), the
implementation will follow the normal/neutral split into a pair of mutually recursion functions
`val_eq` and `neu_eq`, with `neu_eq` synthesizing a type.

```ocaml
let rec val_eq (d : lvl) (eG : tp_env) (v1 : value) (v2 : value) (vA : vtp) : bool =
    match vA, v1, v2 with
    | VStar, VTop, VTop -> true
    | VStar, VStar, VStar -> true
    | VTop, VUnit, VUnit -> true
    | VStar, VPi ((x1, vA1), fB1), VPi ((x2, vA2), fB2) ->
        let v = vvar d in
        val_eq d eG vA1 vA2 VStar && val_eq (d+1) ((x1, vA1)::eG) (fB1 v) (fB2 v) VStar
    | VPi ((x, vA), fB), v1, v2 -> (* handles eta-expansion *)
        let v = vvar d in
        val_eq (d+1) ((x, vA)::eG) (apply v1 v) (apply v2 v) (fB v)
        (* using `apply` to either reduce an application of a lambda or to extend the stack of
           neutral terms *)
    | vA, VN n1, VN n2 ->
        begin match neu_eq d eG n1 n2 with
        | Some _ -> true
        | None -> false
        end
    | _ -> false

(* checking equality of neutral terms needs to compute a type as output, so we use `vtp option` as
an output type to represent "yes they're equal and here's their type" or "no they're not equal". *)
and neu_eq (d : lvl) (eG : tp_env) (n1 : neu) (n2 : neu) : vtp option
    match n1, n2 with
    | NVar l1, NVar l2 when l1 = l2 = Some (List.nth (lvl2ix d l1) eG)
    | NApp (n1, v1), NApp (n2, v2) ->
        begin match neu_eq d eG n1 n2 with
        | Some (VPi ((x, vA), fB)) ->
            if val_eq d eG v1 v2 vA then
                Some (fB v1)
            else
                None
        | Some _ -> failwith "impossible: inputs of neu_eq are ill-typed"
        | None -> None
        end
    | _ -> None
```

Now we can rewrite `check` to use this procedure.

```ocaml
let rec check (d : lvl) (eG : tp_env) (t : tm) (vA : vtp) : unit =
    match vA, t with
    (* ... *)
    | vA, s ->
        let vA' = synth d eG s in
        if not (val_eq d eG vA vA' VStar) then failwith "type mismatch"
```

Much better.

Then there's just one thing left to reflect on in the implementation.

### Checking and evaluating at once?

In our implementation of `check` and `synth`, there are two places where we typecheck a subterm
right before we evaluate it, both involving Pi-types. This begs the question, could we not fuse the
typechecking and evaluation procedures together, to at least handle those cases?

```ocaml
let (let*) x f = Option.bind
let rec eval_check (d : lvl) (eG : tp_env) (e : env) (t : tm) (vA : vtp) : value option =
    match vA, t with
    | VStar, Top -> Some VTop
    | VStar, Star -> Some VStar
    | VStar, Pi ((x, tA), tB) ->
        let* vA = eval_check d eG e tA VStar in
        let* _ = eval_check (d+1) ((x, vA)::eG) ((x, vvar d)::e) tB VStar in
        Some (VPi ((x, vA), fun v -> eval (v::e) tB))
    | VTop, Unit -> VUnit
    | VPi ((_, vA), fB), Lam (x, t) ->
        let* _ = eval_check (d+1) ((x, vA)::eG) ((x, vvar d)::e) t (fB (vvar d)) in
        Some (VLam (x, fun v -> eval (v::e) t))
    | vA, s ->
        let* v, vA' = eval_synth d eG e s in
        if val_eq d eG vA vA' VStar then
            Some v
        else
            None

and eval_synth (d : lvl) (eG : tp_env) (e : env) (t : tm) : (value * vtp) option =
    match t with
    | Var i -> Some (List.nth e i, List.nth eG i)
    | App (s, t) ->
        begin match eval_synth d eG e s with
        | Some (v1, VPi ((x, vA), fB)) ->
            let* v2 = eval_check d eG e t vA in
            Some (apply v1 v2, fB v2)
        | _ -> None (* application subject not a function *)
        end
    | _ -> None (* trying to synthesize from a checkable term *)

and apply v1 v2 = match v1 with
    | VN n -> VN (NApp (n, v2))
    | VLam (_, f) -> f v2
    | _ -> failwith "runtime type error: application subject not a function"
```

This is not an improvement. The point of typechecking is that it's _static_ -- we don't want to run
the program when we typecheck it. Running the program typically takes significantly longer than
merely typechecking it. In the presence of dependent types, we _unfortunately_ have to run some
parts of the program, to do normalization, but that doesn't mean we should go farther and insist on
running the entire program.

## Conclusion

Of the surveyed implementations, the strategy that leans on the semantics avoids the back-and-forth
between syntax and semantics, while also limiting evaluation to only those subterms required as
dependencies in types.

Let's recap that implementation in full, here.

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

type value =
    | VLam of name * (value -> value)
    | VUnit
    | VPi of (name * vtp) * (value -> vtp)
    | VTop
    | VStar
    | VN of neu (* neutral terms embed as values *)

and neu =
    | NVar of lvl
    | NApp of neu * value

and lvl = int

type env = value list

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

type tp_env = env

let vvar l = VN (NVar l)

let rec val_eq (d : lvl) (eG : tp_env) (v1 : value) (v2 : value) (vA : vtp) : bool =
    match vA, v1, v2 with
    | VStar, VTop, VTop -> true
    | VStar, VStar, VStar -> true
    | VTop, VUnit, VUnit -> true
    | VStar, VPi ((x1, vA1), fB1), VPi ((x2, vA2), fB2) ->
        let v = vvar d in
        val_eq d eG vA1 vA2 VStar && val_eq (d+1) ((x1, vA1)::eG) (fB1 v) (fB2 v) VStar
    | VPi ((x, vA), fB), v1, v2 -> (* handles eta-expansion *)
        let v = vvar d in
        val_eq (d+1) ((x, vA)::eG) (apply v1 v) (apply v2 v) (fB v)
        (* using `apply` to either reduce an application of a lambda or to extend the stack of
           neutral terms *)
    | vA, VN n1, VN n2 ->
        begin match neu_eq d eG n1 n2 with
        | Some _ -> true
        | None -> false
        end
    | _ -> false

and neu_eq (d : lvl) (eG : tp_env) (n1 : neu) (n2 : neu) : vtp option
    match n1, n2 with
    | NVar l1, NVar l2 when l1 = l2 = Some (List.nth (lvl2ix d l1) eG)
    | NApp (n1, v1), NApp (n2, v2) ->
        begin match neu_eq d eG n1 n2 with
        | Some (VPi ((x, vA), fB)) ->
            if val_eq d eG v1 v2 vA then
                Some (fB v1)
            else
                None
        | Some _ -> failwith "impossible: inputs of neu_eq are ill-typed"
        | None -> None
        end
    | _ -> None

let rec check (d : lvl) (e : env) (eG : tp_env) (t : tm) (vA : vtp) : unit =
    match vA, t with
    | VStar, Top -> ()
    | VStar, Star -> ()
    | VStar, Pi ((x, tA), tB) ->
        check d e eG tA VStar;
        let vA = eval e tA in
        check (d+1) ((x, vvar d)::e) ((x, vA)::eG) tB VStar
    | VTop, Unit -> ()
    | VPi ((_, vA), fB), Lam (x, t) ->
        check (d+1) ((x, vvar d)::e) ((x, vA)::eG) t (fB (vvar d))
    | vA, s ->
        let vA' = synth d e eG s in
        if not (val_eq d eG vA vA' VStar) then failwith "type mismatch"

and synth (d : lvl) (e : env) (eG : tp_env) (t : tm) : vtp =
    match t with
    | Var i -> List.nth eG i |> snd
    | App (s, t) ->
        begin match synth d eG s with
        | VPi ((x, vA), fB) ->
            check d e eG t vA;
            let v = eval e t in
            fB v
        | _ -> failwith "ill-typed: application subject not a function"
        end
    | _ -> failwith "cannot synthesize type of checkable term"
```

To actually use this implementation, consider that a user will supply a term together with its
type, both represented as syntax and assumed to be closed.

```ocaml
let check_user (t : tm) (tA : tp) : unit =
    check 0 [] [] tA VStar; (* check that the type is a valid type *)
    let vA = eval [] tA in (* evaluate it, to then guide checking of the user's program *)
    check 0 [] [] t vA (* check the user's program *)
```

Depending on the situation, after evaluation, we might simply evaluate the user's program, or
compile it.

This gives, in 115 lines of code, a fairly straightforward implementation of a dependently-typed
lambda calculus. This implementation omits `quote`. After introducing equality directly on values,
it became unnecessary to convert back into syntax to complete the implementation of the type
checker. In a practical implementation, with real error messages, we would need it: in the case
that type checking switches to synthesis, for instance, we check equality of values, and would like
to tell the user what the expected and actual types are!

In the next post in this series, I'll extend the calculus with some built-in inductive types --
natural numbers and equality -- and encode a proof by induction that addition is associative.

---
title: Intro to parsing and parser combinators
---

How do we represent programs? The most obvious way -- the one we're used to -- is what we'd usually
call "source code". Just text. This is a program: `print(2 + 5)`.
Although that's the representation of programs that we're most familiar with as programmers, it
takes a more structured representation to write compilers, interpreters, or linters.

In this article, I'll discuss how programs are more conveniently represented in code, the way we
specify the syntax of programming languages, and finally, a systematic way of turning that
specification into code for the very first stage -- the parser -- of a compiler or interpreter.

## A better way to represent programs

For starters, the textual representation of a program isn't very convenient. It's not immediately
clear how to write an interpreter directly operating on strings of source code.

Instead, let's introduce a different way of representing programs. This new way will address the
major deficiency in representing programs as text, namely that the textual representation doesn't
tell us anything about the _structure_ of the program.

Indeed, viewed as nothing more than a string, `print(2 + 5)` doesn't tell us that `2+5` is the
argument of a function call to `print`, nor that `2` and `5` are the operands of `+`!

This new way of representing programs is called a _syntax tree._ It's called a syntax tree because
it represents a program as a tree data structure according to syntactic rules. Before saying more
about these syntactic rules, let's look at the syntax tree for the program we've been discussing.

<img class="figure figure-natural" src="/figures/2024-08-31-print25-tree.svg">

This representation makes clear the natural nesting inherent to programs. It makes clear that `2`
and `5` are the operands of `+`, and that `2+5` is the argument of the call to `print`.

By making explicit this nesting, the syntax tree representation makes it much easier to implement
an interpreter:

* The leaves of the tree are either integer literals or variables (such as `print`). These are
  simple to interpret: a literal just evaluates to itself, and a variable evaluates to whatever
  value is associated to its name in the current scope.
* The nodes of the tree are _operations._ An operation is also simple to interpret _assuming that
  its operands have already been evaluated._ In this case, interpreting the `+` node just involves
  summing the integer values of the operands.

In other words, once we have our program in a syntax tree representation, an interpreter for such
programs is nothing more than a simple recursive algorithm.

Representing programs as syntax trees makes implementing an interpreter or compiler actually
feasible, but when we _write_ a program, we write _text!_ Therein lies the meat of this
article: the process of turning text into a syntax tree is precisely _parsing._

Before we get to writing a parser, we first need to discuss what exactly a syntax is, and how best
to describe one.

## Syntactic rules

I mentioned "syntactic rules". For a programming language, these form a specification of which
_strings_ might be valid programs. I say "might" because whether `foo(bar + 1)` is a valid
program will depend on what `foo` and `bar` refer to. That, however, is a _semantic_ issue that I
insist we postpone for now.

The way we describe the syntax of the expressions in a programming language is _by induction._

* Base cases: all the _values_ are expressions. In our example, those are just integers.
* Step cases: all the _operations_ are expressions. By "operation" I of course mean things like
  `+`, but also function calls.

A more rigorous but still unsatisfactory way of expressing that inductive definition is to say:

* Integers are expressions.
* If `e1` and `e2` are expressions, then `e1 + e2` is also an expression.
* If `id` is an identifier and `e` is an expression, then `id(e)` (a function call) is also an
  expression.

If programming languages were specified this way, it would require a _lot_ of writing. Fortunately,
way back in the 1960s, John Backus (leader of the team that developed Fortran) and Peter Naur (one
of the developers of the ALGOL programming language), invented a compact notation for writing
such rules that inductively describe syntax: Backus-Naur Form (BNF) grammar.

Here's a BNF grammar for a language that contains the small print-statement program we're working
with.

```
Expression e ::= n | e1 "+" e2 | id "(" e ")"
```

This one and only line of the grammar defines a _syntactic category_ called "expression", whose
elements are referred to by the letter _e,_ possibly with some numbers attached. Next, after the
`::=`, we give a list of different ways we can form expressions, separated by pipes `|`:

1. `n` is an expression. We understand `n` here to mean integer numbers. A more complete
   grammar would include another syntactic category `Number n ::= ...` that explains precisely what
   these numbers are made of.
2. Expressions can be formed also as an expression followed by a `+` followed by another
   expression.
3. An indentifier (represented as `id`), followed by an open parenthesis, followed by an
   expression, followed by a close parenthesis, is also an expression.

Notice that whitespace is completely ignored in this BNF grammar. That's typical.

Defining syntax inductively is a huge boon. With just three defining clauses, a whole slew of
compound expressions are automatically legal. Here are two examples:

* `f(0) + 5`
* `6 + f(1 + g(3 + 4))`

And here's a more... controversial example: `1 + 2 + 3 + 4`.

This example does not have a unique syntax tree! There are several we could draw, following our BNF
grammar.

<img class="figure figure-natural" src="/figures/2024-08-31-add1234.svg">

In other words, our grammar is _ambiguous._ When we turn this grammar into the code for a parser,
we'll resolve this ambiguity.

## From grammar to code

It's straightforward to translate a BNF grammar into a definition for a recursive type in a
strongly typed functional programming language such as OCaml.

```ocaml
type name = string

type exp =
    | Num of int
    | Add of exp * exp
    | Call of name * exp

(* Expression e ::= n | e1 "+" e2 | id "(" e ")" *)
```

* Each syntactic category in the grammar becomes a new type definition.
* Each formation rule for a category becomes a constructor for the type.

What's less straightforward is this: write a function `parse_exp : string -> exp` that converts a
string representation of a program into our fancy syntax tree representation.

First, parsing might fail. There are a great many potential strings we might provide as input, but
only few of these will conform to the grammar. Therefore, we write
`parse_exp : string -> exp option` so the parser can return `None` when it fails.

Next, this type for `parse_exp` is insufficiently general. To see this, imagine how we might try to
implement the parser for addition expressions:

1. Make a recursive call to parse the left operand of plus.
2. Check that there's a `+` at the position after that expression.
3. Make another recursive call to parse the right operand of plus at the position after the `+`.
4. Assemble the both results of the recursive calls into `Add(e1, e2)`.

To implement step 1 of this plan, we would need to make the recursive call `parse_exp` and somehow
figure out after it returns what remains of the input string. With `parse_exp` returning just
`exp option`, we have no way of knowing what remains of the input string.

Let fix that by generalizing `parse_exp`: parse a _prefix_ of the input string, returning both the
expression that makes up that prefix _and_ what's left over from the input string. We can visualize
such a generalized parser like this:

<img class="figure" src="/figures/2024-08-31-legend.svg">

And we can visualize the logic for parsing an addition expression like `2+5` as a chain of parsers:

<img class="figure" src="/figures/2024-08-31-parse-dataflow.svg">

Notice that this sequential chain of parsers also conforms to our vision of what a parser is: the
whole composition takes an input string and outputs a parsed expression together with what's left
of the input string. This is one of the key ideas behind parser _combinators._

## Parser combinators

As the name suggests, parser combinators are ways of combining parsers to produce new ones. 
In this section, we'll explore different ways of combining parsers, and how to implement it all.

Let's make precise the notion of parser that we developed in the previous section by encoding it in
OCaml.

```ocaml
type input = char list
type 'a parser = input -> (input * 'a) option
```

* We replace the usage of `string` with `char list`, since it's easier to take items off the front
  of a list.
* We introduce a type parameter `'a` to enable our parsers to produce different kinds of parsed
  output. Of course, our ultimate goal will be to define an `exp parser` for expressions, but along
  the way we might need other sorts of parsers.

### Sequential composition and identity

In the previous section, I alluded to one way of combining parsers: _sequential composition_ chains
two parsers to that the remaining input returned by first becomes the input consumed by the second.
Let's take a stab at implementing this in OCaml.

```ocaml
let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser = fun input ->
    match p1 input with
    | None -> None
    | Some (input', _) -> p2 input'
```

While this is a workable combinator for several purposes, it doesn't help us to implement the logic
for parsing `"2+5"` from the diagram before. The reason is that `seq p1 p2` throws away the parsed
result of `p1`. In our example, `p1` is "parse subexpression" and returns `Num 2`. That value needs
to find its way into the final value `Add (Num 2, Num 5)`.

We obtain a more powerful implementation, which enables us to use the parsed result of `p1` to
determine what parser to choose next, by making `p2` into a function.

<aside> Any problem in software engineering can be solved by introducing an extra level of
indirection, except for the problem of too many levels of indirection. </aside>

```ocaml
let bind (p : 'a parser) (k : 'a -> 'b parser) : 'b parser = fun input ->
    match p input with
    | None -> None
    | Some (input', x) -> k x input'

(* And seq can be implemented in terms of bind: *)
let seq p1 p2 = bind p1 @@ fun _ -> p2
```

We know from mathematics that whenever we see a composition, we should look for an identity. What
would be such an "identity parser", that when used, "does nothing"?

* Each parser expresses a transformation of the input string; this will need to be an identity.
* Each parser expresses a success/failure choice; this will have to be a success, since a failure
  would cause the composition made from `bind` to abort early.
* Each parser yields some parsed result. Our identity parser will be parametrized by the choice of
  a predetermined "parsed" result to return.

```ocaml
let pure (x : 'a) : 'a parser = fun input -> Some (input, x)
```

Any parser constructed from `pure`:

* Does not change the input string.
* Always succeeds.
* Returns a constant value, independent of the input string.

We can verify that a parser constructed from `pure` really does behave like an identity for `bind`
by doing two quick calculations.

1. `bind (pure x) k` should work out `k x` -- the first parser does nothing so we can simplify to
   just the second parser.
2. `bind p pure` should work out to `p` -- the second parser does nothing, so we again can
   simplify.

```ocaml
(* 1. *) bind (pure x) k
    = fun input -> match (pure x) input with ...
    = fun input -> match Some (input, x) with
                   | Some (input, x) -> k x input
                   | None -> None
    = fun input -> k x input
    = k x

(* 2. *) bind p pure
    = fun input -> match p input with ...
    (* idea: prove that for any `input`,
        `bind p pure input` = `p input` *)
    (* let `input` be some arbitrary input string and proceed by cases: *)

    CASE p input = None:
        match p input with ... = None

    CASE p input = Some (input', x):
        = pure x input'
        = Some (input', x)
```

The second proof is a bit gnarlier since it has to proceed by cases on whether `p` succeeds or
fails. In short, for any choice of `input`, `bind p pure` produces the same outcome as `p`: when
`p` fails on `input`, so does `bind p pure`, but when `p` succeeds on `input`, `bind p pure` also
succeeds and gives the same remaining input and the same parsed result.

Equipped with the power to chain parsers together and to create dummy parsers, we can now create
long chains of parsers that do nothing!

### Parsers that actually _do_ something

The idea of parser combinators is to build a small library of basic parsers and to chain them
together, so we should build a basic parser that can parse a single character.

This parser will take a predicate `char -> bool` that decides whether to accept the character at
the front of the input string. It returns that character as its 'parsed' result. If the input
string is empty, or if the character at the front doesn't satisfy the predicate, then the parser
will fail.

```ocaml
let satisfy (p : char -> bool) : char parser = fun input ->
    match input with
    | [] -> None (* no more input: fail *)
    | c :: cs when p c -> Some (cs, c)
    | _ -> None (* head character doesn't satisfy: fail *)
```

With this, `bind`, and a little recursion, we can define a parser for an exact sequence of
characters. Since the sequence of characters to expect is already known, we'll have the parser
return `unit` as its parsed result.

The nature of this parser will differ from all the ones we've seen so far. Rather than explicitly
examine the `input`, we'll instead build this parser entirely as a composition of existing
combinators.

```ocaml
let rec exact (l : char list) : unit parser = (* notice: no `fun input ->` *)
    match l with
    | [] -> pure ()
    | c :: cs ->
        bind (satisfy @@ fun c' -> c = c') @@ fun _ ->
        exact cs
```

---
title: Intro to parsing and parser combinators
---

How do we represent programs? The most obvious way -- the one we're used to -- is what we'd usually
call "source code". Just text. For instance, this is a program: `print(2 + 5)`.
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
argument of a function call to `print`, or even that `2` and `5` are the operands of `+`!

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
grammar. In other words, our grammar is _ambiguous._ This will become an issue shortly.

<img class="figure figure-natural" src="/figures/2024-08-31-add1234.svg">

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
only few of these will conform to the grammar. Therefore we should write
`parse_exp : string -> exp option`.

Next, this type for `parse_exp` is insufficiently general. To see this, imagine how we might try to
implement the parser for addition expressions:

1. Make a recursive call to parse an expression.
2. Check that there's a `+` at the position after that expression.
3. Make another recursive call to parse an expression at the position after the `+`.
4. Assemble the both results of the recursive calls into `Add(e1, e2)`.

To implement step 1 of this plan, we would need to make the recursive call `parse_exp` and somehow
figure out after it returns what remains of the input string. With `parse_exp` returning just
`exp option`, we have no way of knowing what remains of the input string.

To address this issue, let's generalize `parse_exp` in the following way: have `parse_exp` parse a
_prefix_ of the input string, returning whatever expression is there at the beginning of the
string, _together with_ what's left of the input string.

With this approach, it becomes possible to chain parsing actions together like this:

<img class="figure figure-natural" src="/figures/2024-08-31-parse-dataflow.svg">

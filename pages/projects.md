---------------
title: Projects
---------------

## [Nutcalc](https://github.com/tsani/nutcalc)

Nutcalc is an implementation of the
[inductive model of food](/posts/2025-02-20-induction-on-food.html).
It's a programming language and REPL for describing foods, meals, recipes, meal plans, and a food
journal in a principled, uniform way.

A demo is available [here](https://nutcalc.jerrington.me).

Here's a quick sample of what a Nutcalc program looks like.

```
1 cup 'cooked white rice' weighs 158 g:
- 0.4 g fat + 4.3 g protein + 45 g carbs
- 1.9 mg iron

1 large 'chicken breast' weighs 120 g:
- 4.3 g fat + 37 g protein
- 89 mg sodium + 102 mg cholesterol + 1.2 mg iron + 307 mg potassium

100 g broccoli:
- 0.4 g fat + 7 g carbs + 2.8 g protein
- 33 mg sodium + 316 mg potassium

1 x 'chicken broccoli rice':
- 1 large 'chicken breast' + 2 cup 'cooked white rice' + 200 g broccoli

# ...

1 x Monday:
- 1 x 'oatmeal breakfast'
- 1 x 'eggs sausage bacon toast lunch'
- 1 x 'chicken broccoli rice'
- 1 x 'protein shake'

1 x '2025-02-20':
- 0.5 cup 'dry steel-cut oats' + 50 g walnuts + 2 cup '3.25% milk'
- 4 x 'breakfast sausage' + 4 large egg + 2 slice toast + 2 tsp butter
- 1 medium 'chicken break' + 1 medium 'chicken leg' + 2.5 cup 'cooked white rice' + 200 g broccoli
```

And we can load this into the REPL and query nutrition facts.

```
$ nutcalc -i journal.nut
nutcalc> facts 1 x 'oatmeal breakfast'
energy: 1122.83 kcal
protein: 44.30 g
fat: 46.57 g
carbs: 131.62 g
water: 150.60 g
calcium: 602.41 mg
iron: 4.35 mg
potassium: 930.49 mg
sodium: 84.34 mg
zinc: 2.71 mg
cholesterol: 30.12 mg
nutcalc>
```

## [Eval][]

Eval is an implementation of a strongly and statically typed functional
programming language with type inference, polymorphism, algebraic datatypes, and
pattern matching.

The following program defines linked lists, an eliminator for them, a sample
list, and a function to sum a list, and it calculates the sum of the sample
list.

```
type List a =
  | Nil
  | Cons a (List a)

def rec fold_list : (a -> b -> b) -> List a -> b -> b =
  fun f l e -> match l with
  | Nil -> e
  | Cons x xs -> f x (fold_list f xs e)

def list = Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))))

def sum = fun l -> fold_list (fun x y -> x + y) l 0

def n = sum list
```

## [Apollo][]


Apollo is a web service for downloading music, and integrates with MPD.
Using Apollo's HTTP API, you can send URLs from youtube, soundcloud, etc. to be
downloaded. Apollo comes bundled with bash scripts for submitting URLs from the
command-line.

I have also developed a [Firefox extension][] for Apollo which makes it easy to
submit links and monitor download progress directly in the browser.

## [Computing Workshop][]


Together with my partner [Eric Mayhew](https://emayhew.com/) I create and run a
series of workshops on various topics in computer science. These range from
fundamentals of circuits and programming, to an introduction to machine
learning.

## [servant-pushbullet-client][]

This library describes the Pushbullet API as a type and provides functions
created with [Servant][servant-client] to call the API. It also provides some
miscellaneous functions for dealing with particulars of the Pushbullet API,
such as pagination of results.

This library depends on [another one][pushbullet-types] I wrote which simply
describes the types of objects returned by the Pushbullet API.

Ultimately, the reason I wrote these libraries was to write [tpb][], an
application to interact with the Pushbullet API on the command-line.
On top of this application, I wrote a number of scripts for sending and
receiving SMS on the command-line.

## [servant-github-webhook][]

This is my first published Haskell library. It implements
[Servant][servant-server] combinators to
represent routes that are meant to service GitHub webhooks. These combinators
provide routing based on webhook type and automatic verification of GitHub
digital signatures.

## Vent over Tea

In the spring of 2015,
[Dan Crisan][] and I built the first iteration of the
[Vent over Tea][] web site together.

## Publications

### Harpoon: Mechanizing Metatheory Interactively

[Beluga][beluga] is a proof checker that provides sophisticated infrastructure for
implementing formal systems with the logical framework LF and proving
metatheoretic properties as total, recursive functions transforming LF
derivations. In this paper, we describe Harpoon, an interactive proof engine
built on top of Beluga. It allows users to develop proofs interactively using a
small, fixed set of high-level *actions* that safely transform a subgoal. A
sequence of actions elaborates into a (partial) *proof script* that serves as an
intermediate representation describing an assertion-level proof. Last, a proof
script translates into a Beluga program which can be type-checked independently.

Harpoon is available [on GitHub][harpoon-github] as part of Beluga.
We have used Harpoon to replay a wide array of examples covering all features
supported by Beluga. In particular, we have used it for normalization proofs,
including the recently proposed [POPLMark reloaded challenge][poplmark-reloaded].

This work was presented at CADE 2021. Click [here][harpoon-cade] for the full paper.

The paper lacks many of the details of the full development, which you can read
in my [thesis][].

### The Great Migration and African-American genomic diversity

In 2014 as a research assistant in the lab of [Simon Gravel][], I contributed to [tracts][],
which is a tool for modelling local ancestry patterns along the genome.
Simply put, given a model of migration and the ancestry proportions of
individuals in a present population, tracts evaluates the likelihood of the
migration described by that model having occurred.

Tracts had an issue when presented with large populations that would arise due
to an averaging of the present population's ancestry proportions. I resolved
this by implementing a new demographic model in tracts that performs a model
evaluation on multiple subpopulations and combines the results of those
evaluations. By splitting the population in this way, tracts can avoid
discarding the information given by the variance in the present population's
ancestry proportions.

This new evaluation method was used to produce some of the results in the paper
[_The Great Migration and African-American genomic diversity_][great migration],
of which I am a coauthor.

[servant-server]: https://hackage.haskell.org/package/servant-server
[servant-client]: https://hackage.haskell.org/package/servant-client
[servant-pushbullet-client]: https://github.com/tsani/servant-pushbullet-client
[pushbullet-types]: https://github.com/tsani/pushbullet-types
[servant-github-webhook]: https://github.com/tsani/servant-github-webhook
[Dan Crisan]: http://dancrisan.com/
[Vent over Tea]: http://ventovertea.com/
[Simon Gravel]: http://simongravel.lab.mcgill.ca/Home.html
[tracts]: http://github.com/sgravel/tracts
[great migration]: http://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1006059
[tpb]: https://github.com/tsani/tpb
[Computing Workshop]: https://computing-workshop.com/
[Haskell]: https://haskell.org/
[Hakyll]: https://jaspervdj.be/hakyll/
[Firefox extension]: https://github.com/tsani/apollo-extension/releases
[Apollo]: https://github.com/tsani/apollo
[harpoon-cade]: /pdf/harpoon.pdf
[harpoon-github]: https://github.com/Beluga-lang/Beluga
[poplmark-reloaded]: https://poplmark-reloaded.github.io/
[beluga]: https://www.cs.mcgill.ca/~complogic/beluga/
[thesis]: /pdf/thesis.pdf
[Eval]: https://github.com/tsani/eval

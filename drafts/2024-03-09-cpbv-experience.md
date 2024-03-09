---
title: Using CPBV as a compiler IR
---

Let's face it. Call-by-value is really quite simple. It has its downsides, but it makes sense that CBV is the default. I doubt we'll see languages where the exposed semantics differ much from CBV (notable exception: Haskell).
However, internally in a compiler this may serve as a useful IR. Much as we love the fantasy of every n>1-ary function in ML actually being a unary function returning a function (closure) for the remaining n-1 inputs, during lowering, the compiler must collapse these sequences of functions-returning-closures into n>1-ary functions. This is more efficient because it replaces sequences of "call then return closure then call closure..." (in the stack machine code) with a single "call" operation.
I encountered this exact problem when I wrote an ML compiler -- user defined recursive, polymorphic data types, pattern matching, let-polymorphism, etc. -- targeting a stack machine bytecode. Much code is about about inferring "arities" of functions by counting such sequences of `fun x -> fun y -> fun z -> ...`

---
title: Refactoring Asynchronous Recursion with Continuation-Passing Style
---

(This article was originally drafted in February 2022. The topic is very much
related to the previous article's,
[Implementing environment-based evaluation of recursive functions in OCaml][recursive-closures],
but they can very much be read independently.)

Whew, that's a title that takes some unpacking!
Asynchronous recursion is a concept in JavaScript, and presumably in other
languages with some form of async-await. Maybe another way to call it would
be "indirect recursion". A picture is worth a thousand words, so let me paint
a picture with some code. Let's count down from a given number until we reach
zero, pausing for a second at each recursive call.

```javascript
function countdown(n) {
    if (n <= 0) {
      console.log(
          "No more bottles of beer on the wall, no more bottles of beer!",
      );
      return;
    }
    console.log(`${n} bottles of beer on the wall, ${n} bottles of beer! ...`);
    setTimeout(() => countdown(n - 1), 1000);
}
```

This is what I mean by "asynchronous recursion" or "indirect recursion". Rather
than making a recursive call as a statement of the main body of `countdown`, the
recursive call is made in a callback function to an asynchronous operation -- in
this case, a timeout.

This pattern of recursion can be converted to a kind of continuation-passing
style (CPS). A JavaScript programmer is probably already intimately familiar
with this style of programming. For example in NodeJS, most of the standard
library works this way. Here's an example where we delete a file:

```javascript
const fs = require('fs');

fs.unlink('/tmp/hello', (err) => {
  if (err) { console.log('yikes'); return; }
  console.log('deleted /tmp/hello');
});
```

What's special about this is that the code to execute _after_ the delete has
taken place is represented as a function passed as the second parameter of
`unlink`. The call to `unlink` returns immediately so other work can be
done concurrently. When the deletion finishes, the NodeJS runtime invokes the
callback we passed, and we see either "yikes" or "deleted `/tmp/hello`".

From the internal implementation of `unlink`'s point of view, it has been passed
a function that it can consider a "return function". When `unlink`'s IO
operation is finished, it would like to return back into our code, but because
all of this happened asynchronously, there isn't a stack frame in our code that
we can simply return to! Instead, it calls the "return function" it was passed.

This concept of "return function" is an instance of taking a baked-in notion
of control flow -- in this case, returning from a function call -- and
_reflecting_ it into our code as an explicit function we can call.

To refactor the example of asynchronous recursion I showed earlier, we can apply
this same idea. Let's take the control-flow idea of "making a recursive call"
and reflect it into our code as an explicit function, by rewriting `countdown`
to take an extra parameter which I'll unimaginatively call `recurse`:

```javascript
function countdown(recurse, n) {
    if (n <= 0) {
      console.log(
          "No more bottles of beer on the wall, no more bottles of beer!",
      );
      return;
    }
    console.log(`${n} bottles of beer on the wall, ${n} bottles of beer! ...`);

    recurse(n - 1);
}
```

Notice that I also got rid of the `setTimeout`. The big idea here is that
`countdown` _doesn't care_ what kind of recursion the caller wants. The caller
can choose what to pass as `recurse` and get either synchronous or asynchronous
recursion as desired! The accomplishment here is also a separation of concerns:
we decoupled the behaviour for a single iteration of a recursive loop from the
"loop behaviour".

The change we made to `countdown` comes at a cost, however. What exactly are we
supposed to put as a value for `recurse` when we call `countdown`??

To address this, we will model each kind of recursive behaviour as a separate,
higher-order function to which we can pass `countdown`. The result of applying
such a _recursion combinator_ to `countdown` should be a function that takes all
the "real" parameters of `countdown`, i.e. `n`. Let's figure out how to
implement a "standard" recursion first, then we'll move on to asynchronous
recursion.

```javascript
const recursively = (f) => (...args) => f(recursively(f), ...args);
```

To see how this works, let's evaluate `recursively(countdown)`. There's only
one step to do: substitute `countdown` for `f` and we arrive at
`(...args) => countdown(recursively(countdown), ...args)`.
The result is that when we call _this_ function with a number such as `100`, we
in fact end up calling _countdown_ passing `recursively(countdown)` itself as
the argument for the `recurse` parameter. The process then continues
recursively, we might say.

Finally, to make this asynchronous, we'll need to construct a new `recurse`
function that's more complicated than just `f`. But only a _bit_ more
complicated. It will simply need to call itself within a call to `setTimeout`.

```javascript
const delayedRecursively = (f, delay) =>
    (...args) => f(
        (...args) => setTimeout(
            () => f(delayedRecursively(f, delay), ...args),
            delay,
        ),
        ...args
    );
```

Let's convince ourselves that this works by evaluating `delayedRecursively(countdown, 100)`.
Substituting, we get
```javascript
(...args) => countdown((...args) =>
    setTimeout(() =>
        countdown(delayedRecursively(countdown, 100), ...args), 100), ...args)
```
If we imagine for a moment that JavaScript allows partial application, then we
can substitute further and get the following:
```javascript
(n) => {
    /* ... the countdown implementation ... */
    setTimeout(() => countdown(delayedRecursively(countdown, 100), n-1), 100)
}
```
And if we ran this as `delayedRecursively(countdown, 500)(10)` we would see the
messages printed out slowly.

This approach works! We were able to get different recursive behaviours out of
the same implementation of `countdown`, provided it recurse through an auxiliary
function rather than directly.

There is however a glaring issue with this approach: there is no way for the
recursive call to meaningfully return a value to the caller! Sure, `recursively`
simply returns whatever `f` returns, so one could simply write `const result =
recurse(...);` but what about when we use `delayedRecursively`? We would then
get whatever `setTimeout` returns! To address this, we will need a uniform way
to return a value, that works whether the recursion is synchronous or
asynchronous.

## Returning via yet another function

The trick, as always, is to introduce yet another layer of indirection.

As a motivating example, let's consider a recursive algorithm that sums the
integer values contained in a binary tree.

```javascript
const sumTree = (t) => {
    if (t === null) return 0;
    else return sumTree(t.left) + t.value + sumTree(t.right);
}
```

First and as before, we make the recursion indirect via a `recurse` function.

```javascript
const sumTree = (recurse, t) => {
    if (t === null) return 0;
    else return recurse(t.left) + t.value + recurse(t.right);
}
```

Next, we observe that we have a problem if we use `delayedRecursively`: the
return value of `recurse(t.left)`, for example, won't be the sum of the left
subtree's elements! Let's introduce another function parameter called _resolve_
this time. Rather than returning via the `return` statement, our function will
instead return by calling `resolve`.

```javascript
const sumTree = (recurse, t, resolve) => {
    if (t === null) resolve(0);
    else
        recurse(t.left, (n1) =>
            recurse(t.right, (n2) =>
                resolve(n1 + t.value + n2)));
}
```

What's nice about this approach is that it works without us needing to modify
any of our recursion combinators from the previous section. We can evaluate
`delayedRecursively(sumTree, 100)(sampleTree, (n) => console.log(n))` and it
(slowly) calculates the sum of the tree and prints out the sum.

## Conclusion

In this article, we saw how to separate two concerns that at first glance seem
inextricably tried: the pattern of recursion was isolated from the recursive
algorithm itself. We introduced some combinators, `recursively` and
`delayedRecursively`, to each represent a different pattern of recursion.
Then, we rewrote our recursive algorithm to _recurse via a function_ which we
named `recurse`. That refactored version of the algorithm expresses the base and
step cases of the recursive algorithm without explicitly performing the
recursion, leaving it up to the recursion combinator to decide how exactly that
will be done.
Finally, to account for a desire to return values from our functions, we
introduced one more layer of indirection by returning via a function which we
named `resolve`. No changes to the recursion combinators were necessary to
accommodate this.

The choice of `resolve` for the name of this function is no accident. The code
`recurse(t.left, (n1) => recurse(t.right, (n2) => resolve(n1 + t.value + n2)))`
is honestly gross. It's callback hell. There is certainly a way of writing some
slightly different recursion combinators that take advantage of JavaScript's
promises and `async`/`await` syntax. I leave it to the interested reader to
work this out.

[recursive-closures]: /posts/2023-01-20-recursive-closures.html

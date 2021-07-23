---
title: "Await before returning!"
---

In JavaScript and related languages like TypeScript, we have the wonderful
`async`/`await` syntax to make programming with promises more convenient.
A promise is basically a box that gets filled in by an asynchronous operation,
such as making an HTTP request or reading from a file. Promises support a
natural form of chaining via the `.then()` method, which works like this, for
example in TypeScript:

```typescript
function logPromise(p: Promise<string>): Promise<void> {
    return p.then((msg) => console.log(msg));
}
const p: Promise<string> = Promise.resolve('hello world!');
p.then((msg) => console.log(msg));
```

What's interesting about `p.then(callback)` is that the callback passed to it
can _itself return a promise!_ This is how promises can be chained together: the
promise `p` resolves to a value, so the callback given to `.then()` uses this
value to compute a new promise. We can then continue chaining calls to `.then()`
in such a way that the level of indentation does not increase, thus avoiding
the nested callbacks that lead to callback hell.

What the `async`/`await` syntax gives us is a way to avoid needing to write all
these calls to `.then()`! We can rewrite the above example as this:

```typescript
async function logPromise(p: Promise<string>): Pormise<void> {
    const msg = await p;
    console.log(msg);
}
```
To `await` a promise effectively means to call `.then()` on it and move the rest
of the function into the callback to `.then()`.

What's especially nice about `async`/`await` is that we can use the familiar
try-catch syntax for dealing with exceptions, instead of calling `.catch()` on
the promise.

```typescript
function riskyBusiness() {
  return doAsyncThing().catch((e) => {
    console.log("sorry, no can do.");
    return Promise.reject();
  }).then((x) => doOtherAsyncThing(x)};
}
```

See how unpleasant that is? Instead, we could write something like:

```typescript
async function riskyBusiness() {
  let x;

  try {
    x = await doAsyncThing();
  } catch(e) {
    console.log("Sorry, no can do.");
    throw e;
  }

  return doOtherAsyncThing(x);
}
```

Now notice that I directly returned the result of `doOtherAsyncThing(x)`, which
judging from the function name, is a promise. I instead could have written
`return await doOtherAsyncThing(x)`, but there's no difference: the function
that calls `riskyBusiness()` will need to await whatever `riskyBusiness()`
returns, so whether we await the promise returned by `doOtherAsyncThing(x)`
inside `riskyBusiness()` or outside of it doesn't matter.

Or does it?

There is in fact a situation where it crucially matters whether we put the
`await` there. Consider a slight modification of `riskyBusiness()`:

```typescript
async function riskierBusiness() {
  try {
    return doOtherAsyncThing(await doAsyncThing());
  } catch(e) {
    console.log("Sorry, no can do.");
    throw e;
  }
}
```

Question: **what happens if `doOtherAsyncThing()` throws?**

This seems like a stupid question, doesn't it? Surely we end up inside the
`catch`, since a function called in the `try`-block throws an exception.

And here's precisely where the beauty of `async`/`await` breaks down
somewhat. Merely calling `doOtherAsyncThing()` won't actually cause the
exception to be thrown, since `doOtherAsyncThing()` is asynchronous! The
exception only ends up being thrown when we await the promise computed by
`doOtherAsyncThing()`. So in order to properly capture the exception thrown by
`doOtherAsyncThing()`, we absolutely must await the promise it computes _inside
the `try` block_.

What's especially confusing about this is that nothing changes from the point of
view of the types. To take a simple example, let's write an "async identity
function":

```typescript
function id<T>(x: Promise<T>): Promise<T> {
  return x;
}
```

But there's a second way we could write a function with the same type!

```typescript
async function id2<T>(x: Promise<T>): Promise<T> {
  return await x;
}
```

Since `x: Promise<T>`, the expression `await x` has type `T`. When returning
a value that _isn't_ a promise in an `async` function, the value is implicitly
wrapped up in a promise that simply resolves to the value. Therefore, `return
await x` "unwraps" the promise `x` just to wrap it back up again to be returned.

Hopefully this dive into some of the intricacies `async`/`await` helps you, as it
helped me, to avoid writing subtly incorrect JavaScript.

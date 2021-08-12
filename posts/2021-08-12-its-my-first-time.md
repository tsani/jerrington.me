---
title: It's my first time uwu
---

It happens often enough the _first time_ doing something is the trickiest. A
canonical example is a _thunk_, which is a "value" that needs to be computed the
first time it's accessed, and otherwise yields the computed value. A natural way
to implement such a data structure is this:

```typescript
type Box<T> = { state: 'empty' } | { state: 'full', value: T };

const thunk = <T>(f: () => T): (() => T) => {
    let box: Box<T> = { state: 'empty' };

    return () => {
        switch (box.state) {
            case 'full': return box.value;
            case 'empty':
                const value = f();
                box = { state: 'full', value };
                return value;
        }
    };
};
```

This will work, but it has a few issues. The first one that jumps out is that we
have to inspect the state of the box every time, even though after filling in
the box, the state can never change back to `empty`.
The second issue is subtler, and has to do with garbage collection. The function
returned by `thunk` contains in its closure a reference to `f`, but just like
the empty state of the box, the function `f` is never used again after the first
and only time it's invoked. This means that `f`'s lifetime will extend to at
least the lifetime of the thunk constructed from it, preventing `f` from being
garbage-collected.

We can address both of these issues in one fell swoop by using a "self-replacing
function". The idea is to construct a function that changes its own
implementation when it's invoked. This way we can carefully manage the closure
of the function, and leverage the closure as a data storage mechanism as opposed
to something like a box above.

```typescript
const thunk = <T>(f: () => T): (() => T) => {
    let go = () => {
        const x = f();
        go = () => x;
        return x;
    };
    return () => go();
};
```

This implementation is not only substantially smaller, but it also releases the
reference to `f` after the first invocation of the returned function: the first
time we call `go`, we unconditionally compute `f()` and replace `go` with a
constant function returning `x`, so the only reference kept alive after the
first call is to `x`.

Let's see one more example of this technique, this time to compute the maximum
value of an array. The idea will be to construct a pair of functions, sharing in
their closures a reference to the running maximum. The first function, `input`,
is invoked for each element of the array, and in doing so replaces the function
`get` which retrieves the running maximum.

```typescript
interface Max {
    get: number | null;
    input: (x: number) => void;
}
const makeMax = (): Max => {
    const max: Max = {
        get: () => null;
        input: (x) => {
            max.get = () => x;
            max.input = (y) => {
                if (y > x) x = y;
            };
        },
    };
    return {
        get: () => max.get(),
        input: (x) => max.input(x),
    };
}
```

Then, to compute a maximum of some array of `data: number[]`, we write:
```typescript
const { get, input } = makeMax();
data.forEach(input);
console.log(get());
```

All things considered, this is more complicated than just writing
```typescript
data.reduce<number | null>((max, x) => null === max || x > max ? x : max, null);
```
but there are some subtle annoyances in this:

- We must provide the type parameter `number | null` to `reduce` to avoid a type
  mismatch.
- We must null-check the max on every iteration, even though after the first one
  we know it will never become `null` again.

## The general principle

The underlying phenomenon in each of the examples seen above is _state
change_. Both algorithms begin in an initial empty state: before trying to
read the thunk there is no value, so we must compute it; before seeing any data
items, there is no maximum value. After the first interaction from outside --
invoking the thunk or seeing the first data item -- the algorithm changes
states, thus changing its behaviour: simply yield the computed value or compare
the current input with the previous one to decide the new maximum.

How does this state change become reflected in code? In the first implementation
of `thunk` using a box, the state is explicitly represented as _data_. Therefore
the first thing our function must do when called is figure out what state it's
in by inspecting that data. The same happens in the `reduce` implementation of
max: we must null-check the max to figure out whether we're in the empty state
or the nonempty state.

In contrast, the implementations using self-replacing functions do not need to
inspect the state. Instead, the state change is reflected by an explicit change
in the implementation of the function: as the algorithm changes to a new state,
the driving function(s) of the algorithm change to match their expected
behaviour in that state. A somewhat funny consequence of this is that no
conditions are necessary beyond those that are inherent to the algorithm (good
luck implementing `max` without a condition).

Note that not everything is rainbows and unicorns in this approach: there is a
penalty in using this technique, namely a double indirection.
Notice that in both `thunk` and `makeMax` we define each self-replacing
function separately from a "trampoline function" that "bounces the call" to the
self-replacing function. So we trade an if-statement that decides what state the
algorithm is in with a function call that takes us to the handler for the
current state.

I hope this technique proves useful to you, or at the very least gives you a new
perspective on the internal state of algorithms, and how to reflect these in
your code. Happy hacking!

---
title: Gotos in Python
---

Don't let the `class` keyword fool you; in Python, types are created
dynamically. Besides introducing a new namespace, the `class` syntax merely
establishes a context in which function declarations are given special
treatment.

This class declaration
```
class A:
    def __init__(self):
        self.some_property = "Hello world!"

    def say_hello(self):
        print(self.some_property)

    @property
    def some_property(self):
        return self._some_property

    @some_property.setter
    def some_property(self, value):
        self._some_property = value
```
is in fact equivalent to the following setup using the `type` function
```
def __init__(self):
    self.some_property = "Hello world!"

def say_hello(self):
    print(self.some_property)

@property
def some_property(self):
    return self._some_property

@some_property.setter
def some_property(self, value):
    self._some_property = value

A = type(
        'A',
        tuple(), {
            '__init__': __init__,
            'say_hello': say_hello,
            'some_property': some_property,
        },
)
```

In both cases, the behaviour is the same;
```
a = A()
a.say_hello()
```
will both simply print `Hello world!`.

The `type` function
-------------------

The `type` function can be used in two ways.

First, it can be used to retrieve the type of a value.
```
python> type(a)
<class '__main__.A'>
```

Second, it can be used ot create a new type.
```
python> type('A', tuple(), {})
<class '__main__.A'>
```

You guessed it; the string argument is simply the `__name__` of the new type.
The other two arguments are a bit more mysterious.

The `tuple()` argument represents the base classes that the new type will
inherit from as well as `object`.

Finally, the dictionary argument specifies the methods of the class.

Simulating gotos with exceptions
--------------------------------

With the preliminaries out of the way, we can get to the fun part.

All control flow mechanisms are simply controlled gotos. `try`/`except` is no
different. In the case of exception handling though, the connection is more
obvious: the exception type handled by the `except` block is the label that we
can jump to, and `raise` is the `goto` operator. The problem with just using
built-in exception types like `ValueError` or `RuntimeError` is twofold. First,
if there's any legitimate error-handling logic present, our new goto logic will
trample it. This means that anyone wanting to use exceptions as gotos (God
forbid) will need to pick an exception that they're sure won't be raised by the
normal operation of the code in the `try` block. This can be hard if not
practically impossible.

That's precisely where `type` comes in.

From the above discussion, we know how to create a new class inheriting from
`Exception` that we can use as a one-off that we're guaranteed is not used by
the code inside the `try` block. First let's establish a certain syntactic
sugar.

```
def label(name):
    return type("<label " + name + ">", (Exception,), {})

def goto(label):
    raise label()
```

The `label` function synthesizes a new exception type, and the `goto` function
simply instantiates whatever's given to it and raises it. Here's how we can use
this in practice.

```
done = label('error')
try:
    # a whole lot of code
    # somewhere, nested deep in a bunch of ifs and fors and whiles, a wild goto
    # appears!
    goto(done)
    # lots more code follows
except done:
    print("Done!")
```

Downsides
---------

Gotos are horrible and should never be used in the first place if you can avoid
it.

More seriously though, the major drawback here compared to gotos in C for
instance is that we can't _jump up_. Using these fake gotos, it's only possible
to jump down. Also, there's certainly some interpretation overhead involved in
the `try`/`except` logic. But hey, this is Python, so who cares if it gets a
bit slower?

Related work
------------

It actually turns out that a similar mechanism can be expressed in Haskell, and
that two-way jumping can be achieved. I'm not entirely sure it can be used to
actually control the flow of _execution_, but I've seen something not unlike
this used in an EDSL for representing some kind of assembly language. I
remember the code looking something like the following.

```
mdo
    when someCondition $ goto done
    -- whole bunch of code
    done <- label
    -- some more code
```

Note the `mdo`; this requires `MonadFix`, which I can't fully get my head
around.

I've used a similar approach in my brainfuck compiler. However, because I was
using free monads and free monads don't admit a proper `MonadFix` instance, I
had to resort to a significantly uglier strategy.

[Here](https://github.com/djeik/fuckdown2/blob/master/src/Assembler.hs#L207)'s
what the labels end up looking like when used. The idea behind it was to
provide the primitive operations `NewLabel` and `SetLabel` in the base functor
of my x86 free monad. By separating the creation of labels from the assignment
of their value, I can achieve down jumps. (Otherwise we have in Haskell the
opposite of the problem that we have in Python!)

Internally, what happens in my interpreter for the `AsmF` free monad is that I
use a State monad to track how many labels have been created; each time
`NewLabel` is used, a counter is incremented internally, and the new counter
value is used as a new key added to a mapping from label numbers to addresses
wrapped in `Maybe`. The initial value associated with the new key is `Nothing`.
The `NewLabel` action produces a totally opaque `Label` value that can be
passed around by the code of the free monad. On the other hand, `SetLabel` must
be provided with one such `Label` value. In fact, these `Label`s just contain
the value of the counter at the time they were created, which corresponds to
the key in the map. The interpreter for `SetLabel` knows how many bytes have
been emitted to the output at the time it runs, so it can fill in a `Just`
value in the map for the key wrapped in the provided `Label`.

Of course, a few things can go wrong.

 * We can create a label and forget to use `SetLabel` on it.
 * We can use `SetLabel` more than once on the same label.

What's more is that these problems, although _static_, will blowup at runtime
rather than at compile time. Yuck!

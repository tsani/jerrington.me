---
title: The case against Python
---

Or, _how to implement a switch statement in Python_. Because why not.

Using closures and the decorator syntax, we can make something that looks
reasonably close to a simple switch statement in Python. The idea is this:
we'll design a simple factory function to generate both a `case` function that
can be used as a decorator to register cases in our switch and a `run` function
to run our switch statement.

```
def switch(expr):
    cases = []

    def case(e):
        def wrapper(f):
            cases.append(e, f)
        return wrapper

    def run():
        v = expr()
        for e, body in cases:
            if e == v:
                body()
                break

    return case, run
```

We require that the parameter to `switch` be a callable so that we can delay
evaluating the expression to switch on until the moment the switch is run. In
fact, we might even want to have the generated `run` take the expression to
switch on as a parameter, so that we can reuse the same switch many times on
different values.

Let's see our switch statement in action.

```
if __name__ == '__main__':
    x = input()
    case, run = switch(lambda: x)
    
    @case(5)
    def _():
        print 'kindergarden!'

    @case(18)
    def _():
        print 'university!'

    run()
```

 * If the user inputs `5`, then we print `kindergarden!`
 * If the user inputs `18`, then we print `university!`
 * If the user inputs anything else, then nothing happens.

This trick of using decorators for side effects rather than to perform a
transformation of a function is extremely useful. It's how the Flask framework
registers handlers on routes, for example.

There are some simple improvements that can be made to this.

 * We can implement a default case by returning a third function called
   `default`.
 * We can allow passing unary functions to the `case` function to switch on the
   result of a function. Then `@case(5)` becomes equivalent to
   `@case(lambda x: x == 5)`.

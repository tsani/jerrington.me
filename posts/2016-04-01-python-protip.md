---
title: Python pro-tip #1
---

Suppose you want to access the first element of a sequence but only if the
sequence is non-empty. The obvious way to do it is like this.
```
if some_list:
    pass
    # use some_list[0] to access the first element
```

But there is a subtle problem with this solution. What if `some_list` doesn't
support random access? Maybe it's a generator.

We know that the best way to interact with generators is to use a `for`-loop,
so that's just what we'll do to access the first element.
```
for x in some_list:
    # do something with x
    break
```

The `break` ensures that we won't try to use the subsequent elements of the
generator.

This trick doesn't seem at all useful when explained like this, but let's look
at the situation that made me think of it.

Suppose we have a list of file extensions that we'd like to ignore.
```
IGNORED_EXTENSIONS = ['.pgp', '.asc']
```
We want to get the first such extension that matches at the end of a path and
use its length to strip that many characters from the end of the path.

The naive solution is the following.
```
matched_extensions = [x for x in IGNORED_EXTENSIONS if my_path.endswith(x)]
if matched_extensions:
    my_path = my_path[:-len(matched_extensions[0])]
```

*Yuck*

Using the evidently more elegant loop-based solution we get this.
```
for extension in (x for x in IGNORED_EXTENSIONS if my_path.endswith(x)):
    my_path = my_path[:-len(extension)]
    break
```

In the naive solution, we had to compute the entire list of matched extensions
despite knowing that it will obviously have length one. By using a generator
expression in the second solution, we avoid even _checking_ the other
extensions once a match is found.

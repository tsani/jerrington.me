---
title: Loop once... (f)or else! (Or, Python pro-tip number 2)
---

Once upon a time, some seven years ago, I wrote [a post](/posts/2016-04-01-python-protip) about a Python trick for
accessing the first element of any kind of sequence.

The gist of that article is that just using indexing isn't general enough: if the sequence is
lazily computed by a generator, then indexing won't work.
What does work, however, is to iterate over the sequence using a `for` loop. But since we just want
the first element, let's immediately break out of the loop.

```python
for x in seq:
    print(x) # or do whatever
    break
```

But there's something I missed in that post! What if the sequence is empty?

An obvious solution is to use a boolean:

```python
entered = False
for x in seq:
    entered = True
    print(x)
    break
if not entered:
    pass # do whatever
```

But that's pretty ugly if not outright disgusting.  Instead, we can take advantage of a
little-known feature of Python's for-loops: in Python, a for-loop can have an `else` block! The
semantics are that if the loop terminates _normally,_ then the `else`-block is run; else, if the
loop exits early, e.g.  by a `break`, then the `else`-block is skipped.

These semantics are frankly weird to me, but they end up working out marvellously for our current
use case. It almost makes me think that this is why `else` was allowed in the first place for a
`for`-loop. Consider this.

```python
for x in seq:
    print(x)
    break
else:
    print("nothing in the sequence")
```

If the sequence is nonempty, then we will print the first item in the sequence and abort the loop,
skipping the `else`-block. If the sequence _is_ empty, then **the loop will exit normally,** so the
`else`-block will run!

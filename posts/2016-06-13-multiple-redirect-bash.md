---
title: Redirecting file descriptors to more than one file in bash
---

*TL;DR* make writing to file descriptor three write to standard out and to
standard error simultaneously:

```
exec 3> >(tee >(cat >&2))
```

What the hell was that?
-----------------------

Bash file descriptors are a neat feature. You can open files for reading,
writing, and appending just like in other programming languages. In bash, these
custom open files are given the numeric names `3` through `9`.

Here's how to do it:

  * *Open for writing:* `exec 3> path/to/file`
  * *Open for appending:* `exec 3>> path/to/file`
  * *Open for reading:* `exec 3< path/to/file`

Even though in regular redirections such as `some-command >some-file` we can
include whitespace after the `>`, we can't do that when opening files via
`exec`. Think about it: `exec 3 > path/to/file` would simply be running the
`exec` command with the single argument `3`, redirecting standard output to
`path/to/file`. It makes sense, but it's nonetheless another one of those bash
gotchas.

As for the file we _opened_ with `exec`, `>(tee >(cat >&2))` doesn't exactly
look like a file, now does it? That's because it's a *process substitution*.

There are several kinds of substitutions in bash:

  * *Variable substitution* is probably the most common and is the one we're
    most familiar with; a dollar sign followed by an identifier substitutes the
    value of the variable associated with that identifier for the variable
    substitution expression.
  * *Command substitution* is also fairly common. A dollar sign followed
    immediately by a parenthesized command substitutes the standard output of
    the command for the command substitution expression.
  * *Process substitution* is perhaps the least common of the three, but it's
    the one we use to pull off this trick. A greater-than followed by a
    parenthesized expression runs the inner expression and creates a FIFO
    (under `/dev/fd`) connected to its standard input. Writes to this FIFO are
    sent to the standard input of the command.

There are some other substitutions, such as arithmetic substitution, but it's
no use discussing all of them here.

Back to our command. The outermost process substitution will create a FIFO, say
`/dev/fd/12` connected to the standard input of a `tee` command. `tee` writes
its standard input unchanged to all given files as well as to its standard
output. We give one argument to `tee` in the form of another process
substitution creating another FIFO, say `/dev/fd/13`, this time connected to
`cat`. `cat` concatenates its standard input with all given files, writing the
result to its standard output. We redirect the standard output of `cat` to its
standard error stream, which has been inherited from its parent processes,
meaning that it is the shell's standard error stream.

So what happens when we write to file descriptor three, e.g. with `echo 'hi'
>&3`? Bash will write the standard output of the `echo` command to `/dev/fd/12`
and hence to the standard input of `tee`. `tee` will forward its input to
`/dev/fd/13` and to its standard output (which is the shell's standard output).
`/dev/fd/13` is connected to the standard input of `cat`, which will write its
input unchanged to its standard output; `cat`'s standard output has been
redirected to its standard error, which is the shell's standard error.

Of course, we didn't *need* to open file descriptor three. If we want to write
to more than one file as a one-off, we can do the process substitution on the
spot as follows.

```
echo 'hello world' > >(tee >(cat >&2))
```

However, it can nonetheless be convenient to have a handy shortcut in the form
of `>&3` to mean "write to standard output and to standard error".

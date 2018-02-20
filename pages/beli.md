---
title: Beluga Interactive Mode
---

### Bugs

* Holes are not allowed on the LF level.

  It should be possible to see a list of candidates and choose one, or use the
  logic programming engine to fill it.
  Lots of commands don't make sense on the LF level, like splitting.

* Holes are not allowed in `let` declarations; only in `rec` declarations.

  That's because the type annotation is not considered! Let has to be
  *synthesizable*, not *checkable*.

* Executing a split command with no file loaded results in an unhandled
  exception.

  More generally, I think each command should "just work", without the user
  knowing which commands it depends on.
  So the elisp code for split should:
    - save the current buffer to disk;
    - `%:load` the current buffer into Beli;
    - highlight the holes;
    - finally, call `%:split`.

* Hole overlay is incorrectly positioned

  Upshot: the text under the _overlay_ is deleted and replaced when an
  interactive mode command is executed, e.g. `%: split 0 s` will remove the
  space right before hole 0.

  steps to replicate:

  - open [arithmetic-bug-1.bel][1].
  - run `:beli` to start the Beluga interactive mode.
  - run `:beluga-highlight-holes`.

  [Result](https://files.jerrington.me/arithmetic-bug-1.png).

  In `beluga-mode.el`, there is an alternative implementation of `beluga--pos`
  commented out that is "more efficient".
  However, it gives even worse results: it's totally off for both the line and
  column. But intuitively, this algorithm should really just work: it simply
  moves the cursor to the beginning of the file, and then moves it according to
  the `offset` given by Beluga.

  This suggests to me that the bug lies within the Beluga parser itself.
  
  Update (2018-02-01): After speaking with David and messing around some more,
  we discovered that the "more efficient" algorithm is off by the *number of
  lines*. It appears that line terminators are not counted when calculating the
  offset. We can correct for this in the elisp code by adding the line number of
  the hole to its offset within the file, but the real solution is to fix the
  parser.

  Update (2018-02-01): problem might be in `skip_newlines` in `lexer.ml`;
  there's no `Loc.shift`. Discuss with Aliya regarding the right sequence of
  commands.

### Interactive mode features we should have

* Holes can be named, e.g. `?foo`

  This way, we can use a command like `%: split foo s` to introduce a
  split on variable `s` at the hole named `foo`.
  The user no longer needs to "just know" what the number of all the holes are.

  Anonymous holes `?` can still be supported, and would be implicitly numbered
  as they currently are.
  There is a question about what should happen if a user names a hole with a
  number, e.g. `?0`. A simple solution is to disallow such names by users:
  hole names should be valid identifiers, and identifiers cannot begin with
  numbers.

  When interactive commands generate new holes (e.g. as the result of a split),
  these new holes should also be given names, even if they're simple names like
  `a1`, `a2`, etc. This way the user can see in the file what the hole number
  is.

* Commands automatically start the interactive mode

  (This is low-hanging fruit.)

  The user needs to manually run `:beli` to start the interactive mode.
  Instead, we can add a check to each command to start the interactive mode.
  
  More generally, each interactive mode command should run all the commands it
  depends on, if any, such as saving the buffer to disk, loading it into Beli,
  running `beluga-highlight-holes`, etc.

* Split command detects whether there is a hole at the point, and if any,
  avoids prompting for the hole to split on.

### Confusion

* What's a "line directive" (Camlp4)? Comments in `beluga-mode.el` suggest that
  the `bol` (beginning-of-line) and `offset` values returned by Beluga (and
  ultimately coming from from Camlp4) might not be correct due to these
  directives.
  Would these directives be written into a Beluga source file? Why would
  someone use them?

[1]: https://files.jerrington.me/arithmetic-bug-1.bel

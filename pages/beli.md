---
title: Beluga Interactive Mode
---

### Timeline

* Week of 19 March 2018:
  - named holes: implement smart hole splitting
  - interactive mode bug: splitting not allowed in functions with totality
    annotations.

### Bugs

* Splitting is not allowed in functions with totality annotations.

* Certain files are incorrectly lexed.

  Running `lex_check_all.sh` shows that certain example files are incorrectly
  lexed. These files need to be examined more closely, using `lex_dump` to see
  where the lexer gets mixed up.

* Holes are not allowed on the LF level.

  It should be possible to see a list of candidates and choose one, or use the
  logic programming engine to fill it.
  Lots of commands don't make sense on the LF level, like splitting.

* Holes are not allowed in `let` declarations; only in `rec` declarations.

  That's because the type annotation is not considered! Let has to be
  *synthesizable*, not *checkable*.

  Update (2018-02-26): In other words, it would be possible to write something
  like `let v = ?`, in which case the hole is _invalid_.
  We should syntactically allow writing such holes, but fail at a later stage if
  insufficient type information is available.
  At a minimum, it should be possible to write `let v : Bool = ?`, since the
  type of the hole is known.

  To make this change, I would think to add a production to `cmp_exp_syn`:

  ```
  | "?" -> Comp.Hole _loc
  ```

  But this doesn't work, because Beluga has separate expression AST types for
  expressions that have synthesizable types versus checkable types, so
  `Comp.Hole` constructs a *checkable* AST node, whereas `cmp_exp_syn` needs to
  produce a *synthesizable* AST node.

* Executing a split command with no file loaded results in an unhandled
  exception.

  More generally, I think each command should "just work", without the user
  knowing which commands it depends on.
  So the elisp code for split should:
    - save the current buffer to disk;
    - `%:load` the current buffer into Beli;
    - highlight the holes;
    - finally, call `%:split`.

* **(RESOLVED)** Hole overlay is incorrectly positioned

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
  there's no `Loc.shift`.

  Update: messing around with inserting `Loc.shift` seems to make things
  worse. In particular, line numbers are incorrect.

  Update (2018-02-20): further messing around with `Loc.shift` in the lexer
  seems to make things better. I don't understand why it's behaving now. Now, no
  matter where the hole is in the file, the reported location is only off by
  two. There are situations where it can be off by more, and these arise due to
  the numerous different places where newlines are handled inside the lexer. For
  example, comments have their own newline handling, so inserting comments
  affects the reported hole location.

  Update (2018-02-21): I built a tool to "see what Beluga sees" by dumping the
  token stream constructed by lexing a source file. This tool is now checked in
  to my copy of Beluga, and is called `lex_dump`.

  What I saw amazed me.

  It turns out that all token locations in the sample file I used were
  completely wrong: every token supposedly had a length of 1, and was off by
  one. Strangely, the very first token had a length of zero, and was assigned
  the character span of `0-0`. Notice that this is exactly the character span
  generated by the initial location used to kick-start the lexer. The very first
  call to `Loc.shift` would change this to the span `0-3` by making the new
  start offset the old stop offset and moving the stop offset by the number of
  characters in the lexeme, which in my sample file was `rec`. This suggested to
  me that each token was being assigned the Loc of the *preceding* token. That
  explains the 1-length tokens: those are the spaces between the words in the
  file!

  The bug came from `lexer.ml`, specifically the expression:

  ```
  let tok = Some (lex_token loc_ref lexbuf, !loc_ref) in
  ```

  `lex_token` has side effects: in particular it updates `loc_ref` to refer to
  the right place for the lexeme that is being currently processed. An old
  version of OCaml [supposedly evaluates arguments left-to-right][ocaml-lies],
  but it is said that we should not depend on this. In fact, my OCaml evaluates
  from right to left, which is the only explanation for this behaviour!
  The OCaml specification specifically [leaves unspecified][ocaml-truths] the
  evaluation order for products (and for functions), yet this code in Beluga
  *depended* on a particular evaluation order.
  *Sigh.*

  Update (2018-02-22): I refactored a bunch of the newline handling sprinkled
  throughout the lexer to use a common set of helper functions that correctly
  shift the lexer location and line number. The comment handling is a bit of a
  mess, but I'm slowly starting to understand how this lexer really works.
  I have an idea for a test: using `lex_dump FILE | grep EOI | head -n 1`, we
  can see where Beluga *thinks* the end of the file is, and compare this to
  `wc -c FILE`. If these counts are the same, then (odds are that) the file was
  correctly lexed.

### Interactive mode features we should have

* **(IN PROGRESS)** Holes can be named, e.g. `?foo`

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

  Update (2018-03-19): this feature is implemented and awaiting another PR to be
  merged before I can send a PR for this. The Emacs mode has also been updated
  to allow using hole names instead of numbers. Beluga, when generating new
  holes, does not currently generate names for them, but this is planned for
  later.
  We opted for disallowing hole names that are not valid Beluga identifiers, so
  `?0` is illegal (a lexing error). In fact, we add a new lexical category HOLE,
  which is a question mark followed by a valid identifier.
  Named holes are still numbered, so if a file contains three holes, with the
  second one being named, and the others being anonymous, the third hole is
  still hole number three.

* `beluga-load`, when used internally, affects the current buffer.

  Currently when `beluga-load` is called internally, e.g. because of a split, it
  spawns an interactive prompt for selecting the file to load. I think it's safe
  to assume that when doing a split, we're already in the right buffer, so this
  should be skipped.

* Commands automatically start the interactive mode

  (This is low-hanging fruit.)

  The user needs to manually run `:beli` to start the interactive mode.
  Instead, we can add a check to each command to start the interactive mode.

  More generally, each interactive mode command should run all the commands it
  depends on, if any, such as saving the buffer to disk, loading it into Beli,
  running `beluga-highlight-holes`, etc.

  Update (2018-03-19): This is implemented for splitting. When invoking
  `beluga-split-hole`, its prerequisite commands `beluga-load` and
  `beluga-highlight-holes` will both be called.
  However, we still don't actually *start* the interactive mode. We merely
  invoke the prerequisite commands.

* Smart hole splitting:
  split command detects whether there is a hole at the point, and if any, avoids
  prompting for the hole to split on.

  Update (2018-02-26): I was thinking about whether this is actually possible,
  and it turns out that it is. We can model this functionality after the
  `describe-function` Emacs builtin, which uses the word under the cursor as the
  default value. This will tie in very nicely with the named holes feature,
  since we can offer an interactive prompt that would look like
  `Hole to split on (default foo):`
  if the cursor is over a hole `?foo`.

  This depends on the named holes feature, which is now essentially complete.

### Confusion

None at the moment!

[1]: https://files.jerrington.me/arithmetic-bug-1.bel
[ocaml-lies]: https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora029.html
[ocaml-truths]: https://caml.inria.fr/pub/docs/manual-ocaml/expr.html#sec144

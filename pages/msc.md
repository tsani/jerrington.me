---
title: "M.Sc. work"
---

This is a living document. As due work items arise and as tasks are completed,
this page will be updated to reflect my progress.

## Reading

For each reading, I will produce a short document with my notes on it and the
key take-aways.

### Suggested readings

* _Focused Inductive Theorem Proving_
  by D. Baelde, D. Miller, Z. Snow.

  **Expected read by: 19 January 2018**

  **Done on 22 January 2018.** [Notes.][fitp]

* _Programming with Binders and Indexed Data-Types_
  by A. Cave and B. Pientka.

  _In progress._ **Expected read by: 31 January 2018.**

  [Notes.][pbidt]

* _Focusing and Higher-Order Abstract Syntax_
  by N. Zeilberger.

  **Expected read by: 2 February 2018.**

* _Which types have a unique inhabitant?_
  by G. Scherer.

  Reading only sections 7.2, 7.3, and 10.1.

  **Expected read by: 7 February 2018.**

* _Focusing on Pattern Matching_
  by N. Krishnaswami.

* _Automated Theorem Proving in a Simple Meta Logic for LF_
  by C. Schurmann and F. Pfenning.

## Implementation

* Fix the interactive mode bugs Aliya and I discussed.

  **Due: 9 February 2018**

## Code review

* Chart out the code paths for the Beluga interactive development loop:
    * user writes holes in their code
    * program text is parsed and reconstructed into an AST
    * user interactive command: split on a hole
    * Beluga splits the hole internally and refines the AST
    * Beluga erases implicit parameters
    * Beluga pretty-prints the result
    * Beluga reparses and reconstructs the result into an AST

  **Expected due date: 2 February 2018.**

* Investigate existing higher-order logic programming engine implemented in
  Beluga but that isn't hooked up.

  **Expected due date: 7 February 2018.**

* Investigate Agda's interactive mode testing framework.

  **Expected due date: 1 February 2018.**

## Planning & Design

* Design an interactive mode testing framework for Beluga.

  Expected output: document outlining the structure of the framework and how to
  use it.

  **Expected due date: 1 February 2018.**

## Meetings & knowledge sharing

* Meet with Aliya to learn about her findings w.r.t Beluga interactive mode.

  **Done: 11 January 2018.**

* Meet with Szilvia to share knowledge about Beluga interactive mode and
  outline responsibilities in implementation / testing of future interactive
  mode improvements.

# Work log

* 17 January 2018: started reading _Focused Inductive Theorem Proving_.
* 18 January 2018: continued reading.
* 19 January 2018:
  * continued reading,
  * set up Beluga development environment.
* 22-26 January 2018:
  * busy week due to other commitments.
  * completed reading [_Focused Inductive Theorem Proving_][fitp].
  * started reading [_Programming with Binders and Indexed Data-Types_][pbidt].
* 29 January 2018:
  * started familiarizing myself with Emacs

[pbidt]: /notes/programming-binders-indexed-data-types.html
[fitp]: /notes/focused-inductive-atp.html

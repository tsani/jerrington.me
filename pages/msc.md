---
title: "Jake M.Sc. work"
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

* _Programming with Binders and Indexed Data-Types_
  by A. Cave and B. Pientka.

  **Expected read by: 19 January 2018**

* _Focusing and Higher-Order Abstract Syntax_
  by N. Zeilberger.

  **Expected read by: 26 January 2018**

* _Focusing on Pattern Matching_
  by N. Krishnaswami.

  **Expected read by: 26 January 2018**

* _Automated Theorem Proving in a Simple Meta Logic for LF_
  by C. Schurmann and F. Pfenning.

  **Expected read by: 26 January 2018**

## Code review

* Chart out the code paths for the Beluga interactive development loop:
    * user writes holes in their code
    * program text is parsed and reconstructed into an AST
    * user interactive command: split on a hole
    * Beluga splits the hole internally and refines the AST
    * Beluga erases implicit parameters
    * Beluga pretty-prints the result
    * Beluga reparses and reconstructs the result into an AST

  **Expected due date: 31 January 2018.**

* Investigate existing higher-order logic programming engine implemented in
  Beluga but that isn't hooked up.

  **Expected due date: 7 February 2018.**

* Investigate Agda's interactive mode testing framework.

  **Expected due date: 9 February 2018.**

## Planning & Design

* Design an interactive mode testing framework for Beluga.
  Expected output: document outlining the structure of the framework and how to
  use it.

  **Expected due date: 15 February 2018.**

## Meetings & knowledge sharing

* Meet with Aliya to learn about her findings w.r.t Beluga interactive mode.

  **Done: 11 January 2018.**
